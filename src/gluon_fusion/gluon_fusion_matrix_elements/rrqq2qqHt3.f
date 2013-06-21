  
      subroutine rrqq2qqht3
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrqq2qqH31J1  
      doubleprecision rrqq2qqH31J2  
      doubleprecision rrqq2qqH31J3  
      doubleprecision rrqq2qqht3s1e1  
      doubleprecision rrqq2qqht3s1e0  
      doubleprecision rrqq2qqht3s1em1  
      doubleprecision rrqq2qqht3s1em2  
      doubleprecision rrqq2qqht3s1em3  
      doubleprecision rrqq2qqht3s1em4  
      doubleprecision rrqq2qqht3s2e1  
      doubleprecision rrqq2qqht3s2e0  
      doubleprecision rrqq2qqht3s2em1  
      doubleprecision rrqq2qqht3s2em2  
      doubleprecision rrqq2qqht3s2em3  
      doubleprecision rrqq2qqht3s2em4  
      doubleprecision rrqq2qqht3s3e1  
      doubleprecision rrqq2qqht3s3e0  
      doubleprecision rrqq2qqht3s3em1  
      doubleprecision rrqq2qqht3s3em2  
      doubleprecision rrqq2qqht3s3em3  
      doubleprecision rrqq2qqht3s3em4  
      doubleprecision rrqq2qqht3s4e1  
      doubleprecision rrqq2qqht3s4e0  
      doubleprecision rrqq2qqht3s4em1  
      doubleprecision rrqq2qqht3s4em2  
      doubleprecision rrqq2qqht3s4em3  
      doubleprecision rrqq2qqht3s4em4  
      doubleprecision rrqq2qqht3s5e1  
      doubleprecision rrqq2qqht3s5e0  
      doubleprecision rrqq2qqht3s5em1  
      doubleprecision rrqq2qqht3s5em2  
      doubleprecision rrqq2qqht3s5em3  
      doubleprecision rrqq2qqht3s5em4  
      doubleprecision rrqq2qqht3s6e1  
      doubleprecision rrqq2qqht3s6e0  
      doubleprecision rrqq2qqht3s6em1  
      doubleprecision rrqq2qqht3s6em2  
      doubleprecision rrqq2qqht3s6em3  
      doubleprecision rrqq2qqht3s6em4  
      doubleprecision rrqq2qqht3s7e1  
      doubleprecision rrqq2qqht3s7e0  
      doubleprecision rrqq2qqht3s7em1  
      doubleprecision rrqq2qqht3s7em2  
      doubleprecision rrqq2qqht3s7em3  
      doubleprecision rrqq2qqht3s7em4  
      doubleprecision rrqq2qqht3s8e1  
      doubleprecision rrqq2qqht3s8e0  
      doubleprecision rrqq2qqht3s8em1  
      doubleprecision rrqq2qqht3s8em2  
      doubleprecision rrqq2qqht3s8em3  
      doubleprecision rrqq2qqht3s8em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrqq2qqht3s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqq2qqht3s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrqq2qqht3s3e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrqq2qqht3s4e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rrqq2qqht3s5e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rrqq2qqht3s6e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=rrqq2qqht3s7e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=rrqq2qqht3s8e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrqq2qqht3s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqq2qqht3s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrqq2qqht3s3e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrqq2qqht3s4e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rrqq2qqht3s5e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rrqq2qqht3s6e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=rrqq2qqht3s7e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=rrqq2qqht3s8e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrqq2qqht3s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqq2qqht3s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrqq2qqht3s3em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrqq2qqht3s4em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rrqq2qqht3s5em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rrqq2qqht3s6em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=rrqq2qqht3s7em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=rrqq2qqht3s8em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrqq2qqht3s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqq2qqht3s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrqq2qqht3s3em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrqq2qqht3s4em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rrqq2qqht3s5em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rrqq2qqht3s6em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=rrqq2qqht3s7em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=rrqq2qqht3s8em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrqq2qqht3s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqq2qqht3s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrqq2qqht3s3em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrqq2qqht3s4em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rrqq2qqht3s5em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rrqq2qqht3s6em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=rrqq2qqht3s7em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=rrqq2qqht3s8em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrqq2qqht3s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqq2qqht3s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrqq2qqht3s3em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrqq2qqht3s4em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rrqq2qqht3s5em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rrqq2qqht3s6em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=rrqq2qqht3s7em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=rrqq2qqht3s8em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrqq2qqht3s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.3141592653589793D1 ** 2
      t5 = lh ** 2
      t7 = -0.30D2 * t3 + 0.180D3 * t5
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
      t32 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t40 = -0.2884936567583026D3 - 0.120D3 * t5 * lh + 0.60D2 * lh * t3
      t41 = 0.3141592653589793D1 * t40
      t46 = t23 * t19 * 0.3141592653589793D1
      t50 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t54 = t3 ** 2
      t55 = t5 ** 2
      t67 = t23 ** 2
      t72 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t76 = 0.3141592653589793D1 * t27
      t77 = 0.1D1 - x4
      t78 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # t77)
      t79 = x1 ** 2
      t80 = t79 * t11
      t81 = t80 * t13
      t82 = t16 * x4
      t83 = -t77
      t84 = t82 * t83
      t87 = log(-0.4D1 * t81 * t84)
      t88 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # t77)
      t90 = t87 ** 2
      t91 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # t77)
      t94 = t13 * t16
      t95 = t94 * x4
      t98 = log(0.4D1 * t80 * t95)
      t100 = t98 ** 2
      t107 = 0.3141592653589793D1 * lh
      t108 = t27 * t31
      t115 = t72 - t91
      t116 = t108 * t115
      t119 = 0.1D1 / x1
      t121 = 0.1D1 / x4
      t124 = t80 * t94
      t126 = log(0.4D1 * t124)
      t132 = t126 ** 2
      t142 = t108 * t72
      t143 = t41 * t142
      t154 = x3 * t79
      t155 = t154 * t11
      t158 = log(0.4D1 * t155 * t95)
      t160 = x4 * t83
      t164 = log(-0.4D1 * t155 * t94 * t160)
      t171 = -t108 * t115
      t175 = 0.1D1 / x3
      t177 = t119 * t121
      t180 = t154 * t17
      t182 = log(0.4D1 * t180)
      t184 = t182 ** 2
      t203 = log(0.4D1 * t14 * t82)
      t207 = log(-0.4D1 * t14 * t84)
      t213 = t207 ** 2
      t220 = t203 ** 2
      t244 = x3 * t11
      t245 = t244 * t13
      t248 = log(-0.4D1 * t245 * t84)
      t250 = t248 ** 2
      t255 = log(0.4D1 * t244 * t95)
      t257 = t255 ** 2
      t277 = log(0.4D1 * t244 * t94)
      t283 = t277 ** 2
      t303 = (t8 + 0.180D3 * t20 * lh + 0.45D2 * t24) * t27 * t31 * t32 
     #/ 0.1440D4 + (t41 - t20 * t7 - 0.90D2 * t24 * lh - 0.15D2 * t46) *
     # t27 * t31 * t50 / 0.1440D4 + (0.3141592653589793D1 * (t54 + 0.60D
     #2 * t55 + 0.5769873135166051D3 * lh - 0.60D2 * t5 * t3) - t20 * t4
     #0 + t24 * t7 / 0.2D1 + 0.30D2 * t46 * lh + 0.15D2 / 0.4D1 * t67 * 
     #0.3141592653589793D1) * t27 * t31 * t72 / 0.1440D4 + (0.90D2 * t76
     # * t31 * (-t78 + t87 * t88 - t90 * t91 / 0.2D1 + t32 - t98 * t50 +
     # t100 * t72 / 0.2D1) - 0.180D3 * t107 * t108 * (-t88 + t87 * t91 +
     # t50 - t98 * t72) + t8 * t116) * t119 * t121 / 0.720D3 + (t8 * t10
     #8 * (t50 - t126 * t72) + 0.90D2 * t76 * t31 * (-t126 * t32 + t132 
     #* t50 / 0.2D1 - t132 * t126 * t72 / 0.6D1) + t143 - 0.180D3 * t107
     # * t108 * (t32 - t126 * t50 + t132 * t72 / 0.2D1)) * t119 / 0.720D
     #3 - (0.90D2 * t76 * t31 * (-t50 + t158 * t72 + t88 - t164 * t91) -
     # 0.180D3 * t107 * t171) * t175 * t177 / 0.720D3 - (0.90D2 * t76 * 
     #t31 * (-t32 + t182 * t50 - t184 * t72 / 0.2D1) - 0.180D3 * t107 * 
     #t108 * (-t50 + t182 * t72) - t8 * t142) * t175 * t119 / 0.720D3 + 
     #(t8 * t108 * (t50 - t203 * t72 - t88 + t207 * t91) + 0.90D2 * t76 
     #* t31 * (t207 * t78 - t213 * t88 / 0.2D1 + t213 * t207 * t91 / 0.6
     #D1 - t203 * t32 + t220 * t50 / 0.2D1 - t220 * t203 * t72 / 0.6D1) 
     #+ t41 * t116 - 0.180D3 * t107 * t108 * (t32 - t203 * t50 + t220 * 
     #t72 / 0.2D1 - t78 + t207 * t88 - t213 * t91 / 0.2D1)) * t121 / 0.1
     #440D4 - (0.90D2 * t76 * t31 * (t78 - t248 * t88 + t250 * t91 / 0.2
     #D1 - t32 + t255 * t50 - t257 * t72 / 0.2D1) - 0.180D3 * t107 * t10
     #8 * (t88 - t248 * t91 - t50 + t255 * t72) + t8 * t171) * t175 * t1
     #21 / 0.1440D4 + (t8 * t108 * (t50 - t277 * t72) + 0.90D2 * t76 * t
     #31 * (-t277 * t32 + t283 * t50 / 0.2D1 - t283 * t277 * t72 / 0.6D1
     #) + t143 - 0.180D3 * t107 * t108 * (t32 - t277 * t50 + t283 * t72 
     #/ 0.2D1)) * t175 / 0.1440D4
      t304 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t303)
      t306 = 0.1D1 - x1
      t307 = 0.1D1 - x3
      t308 = KAPPA2(t306, x2, t307, 0.10D1, z)
      t309 = s * t308
      t310 = -t306
      t311 = t1 * t310
      t312 = -t307
      t313 = t311 * t312
      t315 = t311 * x3
      t317 = t1 * x1
      t319 = t308 ** 2
      t322 = t310 * x1
      t326 = 0.1D1 / (-0.2D1 + t308)
      t327 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, t306, x2, t307, 0.
     #10D1)
      t328 = t326 * t327
      t329 = t154 * t14
      t330 = t310 ** 2
      t331 = t16 * t330
      t332 = t312 * x4
      t333 = t319 ** 2
      t338 = log(-0.4D1 * t329 * t331 * t332 * t333)
      t340 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, t306, x2, t307, 0.
     #10D1)
      t346 = t107 * t27
      t348 = t31 * t326 * t340
      t354 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, t306, x2, t307, 0.
     #10D1)
      t360 = log(-0.4D1 * t329 * t331 * t312 * t333)
      t361 = t360 * t326
      t363 = t360 ** 2
      t376 = t8 * t27
      t382 = -(0.90D2 * t76 * t31 * (t328 - t338 * t326 * t340) - 0.180D
     #3 * t346 * t348) * t175 * t177 / 0.720D3 - (0.90D2 * t76 * t31 * (
     #t326 * t354 - t361 * t327 + t363 * t326 * t340 / 0.2D1) - 0.180D3 
     #* t107 * t108 * (t328 - t361 * t340) + t376 * t348) * t175 * t119 
     #/ 0.720D3
      t383 = FJET(XB1, XB2, s, t309 * t313, -t309 * t315, t309 * t317, 0
     #.0D0, -s * t319 * t15 * t322 * x3, t382)
      t385 = KAPPA2(t306, x2, t307, t77, z)
      t386 = s * t385
      t389 = t317 * t83
      t391 = t317 * x4
      t393 = t385 ** 2
      t398 = cos(t9)
      t401 = Sqrt(x3 * t312 * t160)
      t408 = 0.1D1 / (-0.2D1 + t385)
      t409 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, t306, x2, t307, t7
     #7)
      t412 = t393 ** 2
      t417 = log(0.4D1 * t180 * t330 * t312 * t160 * t412)
      t419 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, t306, x2, t307, t7
     #7)
      t429 = 0.90D2 * t76 * t31 * (-t408 * t409 + t417 * t408 * t419) + 
     #0.180D3 * t346 * t31 * t408 * t419
      t433 = FJET(XB1, XB2, s, t386 * t313, -t386 * t315, -t386 * t389, 
     #t386 * t391, s * t393 * t15 * t322 * (-x3 - x4 + 0.2D1 * x3 * x4 +
     # 0.2D1 * t398 * t401), -t429 * t175 * t177 / 0.720D3)
      t441 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, t306, x2, 0.10D1, 
     #0.10D1)
      t445 = log(0.4D1 * t81 * t331 * x4)
      t446 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, t306, x2, 0.10D1, 
     #0.10D1)
      t448 = t445 ** 2
      t449 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, t306, x2, 0.10D1, 
     #0.10D1)
      t461 = t108 * t449
      t462 = t8 * t461
      t466 = t94 * t330
      t469 = log(0.4D1 * t80 * t466)
      t475 = t469 ** 2
      t495 = t330 * x4
      t499 = log(0.4D1 * t155 * t94 * t495)
      t512 = log(0.4D1 * t155 * t466)
      t514 = t512 ** 2
      t530 = (0.90D2 * t76 * t31 * (-t441 + t445 * t446 - t448 * t449 / 
     #0.2D1) - 0.180D3 * t107 * t108 * (-t446 + t445 * t449) - t462) * t
     #119 * t121 / 0.720D3 + (-t8 * t108 * (t446 - t469 * t449) - 0.90D2
     # * t76 * t31 * (-t469 * t441 + t475 * t446 / 0.2D1 - t475 * t469 *
     # t449 / 0.6D1) - t41 * t461 + 0.180D3 * t107 * t108 * (t441 - t469
     # * t446 + t475 * t449 / 0.2D1)) * t119 / 0.720D3 - (0.90D2 * t76 *
     # t31 * (t446 - t499 * t449) - 0.180D3 * t107 * t461) * t175 * t177
     # / 0.720D3 - (0.90D2 * t76 * t31 * (t441 - t512 * t446 + t514 * t4
     #49 / 0.2D1) - 0.180D3 * t107 * t108 * (t446 - t512 * t449) + t462)
     # * t175 * t119 / 0.720D3
      t531 = FJET(XB1, XB2, s, -t2 * t310, 0.0D0, t2 * x1, 0.0D0, 0.0D0,
     # t530)
      t535 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t307, 
     #0.10D1)
      t536 = t16 * t312
      t540 = log(-0.4D1 * t245 * t536 * x4)
      t541 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t307, 
     #0.10D1)
      t543 = t540 ** 2
      t544 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t307, 
     #0.10D1)
      t547 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t307, 
     #t77)
      t548 = t536 * t160
      t551 = log(0.4D1 * t245 * t548)
      t552 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t307, 
     #t77)
      t554 = t551 ** 2
      t555 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t307, 
     #t77)
      t569 = t108 * (-t555 + t544)
      t575 = t94 * t312
      t578 = log(-0.4D1 * t244 * t575)
      t584 = t578 ** 2
      t594 = t108 * t544
      t608 = log(0.4D1 * t329 * t548)
      t613 = log(-0.4D1 * t155 * t94 * t332)
      t627 = log(-0.4D1 * t155 * t575)
      t629 = t627 ** 2
      t646 = -(0.90D2 * t76 * t31 * (t535 - t540 * t541 + t543 * t544 / 
     #0.2D1 - t547 + t551 * t552 - t554 * t555 / 0.2D1) - 0.180D3 * t107
     # * t108 * (t541 - t540 * t544 - t552 + t551 * t555) + t8 * t569) *
     # t175 * t121 / 0.1440D4 + (-t8 * t108 * (t541 - t578 * t544) - 0.9
     #0D2 * t76 * t31 * (-t578 * t535 + t584 * t541 / 0.2D1 - t584 * t57
     #8 * t544 / 0.6D1) - t41 * t594 + 0.180D3 * t107 * t108 * (t535 - t
     #578 * t541 + t584 * t544 / 0.2D1)) * t175 / 0.1440D4 - (0.90D2 * t
     #76 * t31 * (-t552 + t608 * t555 + t541 - t613 * t544) - 0.180D3 * 
     #t107 * t569) * t175 * t177 / 0.720D3 - (0.90D2 * t76 * t31 * (t535
     # - t627 * t541 + t629 * t544 / 0.2D1) - 0.180D3 * t107 * t108 * (t
     #541 - t627 * t544) + t8 * t594) * t175 * t119 / 0.720D3
      t647 = FJET(XB1, XB2, s, -t2 * t312, t2 * x3, 0.0D0, 0.0D0, 0.0D0,
     # t646)
      t649 = KAPPA2(t306, x2, 0.10D1, t77, z)
      t650 = s * t649
      t654 = t649 ** 2
      t660 = 0.1D1 / (-0.2D1 + t649)
      t661 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, t306, x2, 0.10D1, 
     #t77)
      t663 = t654 ** 2
      t668 = log(-0.4D1 * t124 * t495 * t83 * t663)
      t669 = t668 * t660
      t670 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, t306, x2, 0.10D1, 
     #t77)
      t672 = t668 ** 2
      t674 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, t306, x2, 0.10D1, 
     #t77)
      t681 = t660 * t670
      t688 = t31 * t660 * t674
      t697 = log(-0.4D1 * t329 * t331 * t160 * t663)
      t710 = (-0.90D2 * t76 * t31 * (t660 * t661 - t669 * t670 + t672 * 
     #t660 * t674 / 0.2D1) + 0.180D3 * t107 * t108 * (t681 - t669 * t674
     #) - t376 * t688) * t119 * t121 / 0.720D3 - (0.90D2 * t76 * t31 * (
     #t681 - t697 * t660 * t674) - 0.180D3 * t346 * t688) * t175 * t177 
     #/ 0.720D3
      t711 = FJET(XB1, XB2, s, -t650 * t311, 0.0D0, -t650 * t389, t650 *
     # t391, -s * t654 * t15 * t322 * x4, t710)
      rrqq2qqht3s1e1 = t304 * t303 + t383 * t382 - t433 * t429 * t175 * 
     #t119 * t121 / 0.720D3 + t531 * t530 + t647 * t646 + t711 * t710

      end function



      doubleprecision function rrqq2qqht3s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.3141592653589793D1 * lh
      t5 = x2 * 0.3141592653589793D1
      t6 = sin(t5)
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
      t24 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t28 = 0.3141592653589793D1 ** 2
      t30 = lh ** 2
      t32 = -0.30D2 * t28 + 0.180D3 * t30
      t33 = 0.3141592653589793D1 * t32
      t36 = t15 ** 2
      t37 = t36 * 0.3141592653589793D1
      t41 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t59 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t63 = t19 * 0.3141592653589793D1
      t64 = 0.1D1 - x4
      t65 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # t64)
      t66 = x3 * t7
      t67 = t66 * t9
      t68 = t12 * x4
      t69 = -t64
      t70 = t68 * t69
      t73 = log(-0.4D1 * t67 * t70)
      t74 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # t64)
      t76 = t9 * t12
      t77 = t76 * x4
      t80 = log(0.4D1 * t66 * t77)
      t86 = t19 * t23
      t87 = t74 - t59
      t92 = 0.1D1 / x3
      t94 = 0.1D1 / x4
      t99 = log(0.4D1 * t66 * t76)
      t101 = t99 ** 2
      t113 = t86 * t59
      t114 = t33 * t113
      t120 = log(0.4D1 * t10 * t68)
      t122 = t120 ** 2
      t125 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, t64)
      t128 = log(-0.4D1 * t10 * t70)
      t130 = t128 ** 2
      t144 = -t86 * t87
      t149 = x1 ** 2
      t150 = t149 * t7
      t151 = t150 * t9
      t154 = log(-0.4D1 * t151 * t70)
      t158 = log(0.4D1 * t150 * t77)
      t167 = 0.1D1 / x1
      t171 = t63 * t23
      t173 = t167 * t94
      t177 = x3 * t149
      t180 = log(0.4D1 * t177 * t13)
      t192 = t150 * t76
      t194 = log(0.4D1 * t192)
      t196 = t194 ** 2
      t211 = (-0.180D3 * t3 - 0.90D2 * t16) * t19 * t23 * t24 / 0.1440D4
     # + (t33 + 0.180D3 * t16 * lh + 0.45D2 * t37) * t19 * t23 * t41 / 0
     #.1440D4 + (0.3141592653589793D1 * (-0.2884936567583026D3 - 0.120D3
     # * t30 * lh + 0.60D2 * lh * t28) - t16 * t32 - 0.90D2 * t37 * lh -
     # 0.15D2 * t36 * t15 * 0.3141592653589793D1) * t19 * t23 * t59 / 0.
     #1440D4 - (0.90D2 * t63 * t23 * (t65 - t73 * t74 - t41 + t80 * t59)
     # - 0.180D3 * t3 * t86 * t87) * t92 * t94 / 0.1440D4 + (0.90D2 * t6
     #3 * t23 * (t24 - t99 * t41 + t101 * t59 / 0.2D1) - 0.180D3 * t3 * 
     #t86 * (t41 - t99 * t59) + t114) * t92 / 0.1440D4 + (0.90D2 * t63 *
     # t23 * (t24 - t120 * t41 + t122 * t59 / 0.2D1 - t125 + t128 * t65 
     #- t130 * t74 / 0.2D1) - 0.180D3 * t3 * t86 * (t41 - t120 * t59 - t
     #65 + t128 * t74) + t33 * t144) * t94 / 0.1440D4 + (0.90D2 * t63 * 
     #t23 * (-t65 + t154 * t74 + t41 - t158 * t59) - 0.180D3 * t3 * t144
     #) * t167 * t94 / 0.720D3 - t171 * t87 * t92 * t173 / 0.8D1 - (0.90
     #D2 * t63 * t23 * (-t41 + t180 * t59) + 0.180D3 * t3 * t113) * t92 
     #* t167 / 0.720D3 + (0.90D2 * t63 * t23 * (t24 - t194 * t41 + t196 
     #* t59 / 0.2D1) - 0.180D3 * t3 * t86 * (t41 - t194 * t59) + t114) *
     # t167 / 0.720D3
      t212 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t211)
      t214 = 0.1D1 - x1
      t215 = 0.1D1 - x3
      t216 = KAPPA2(t214, x2, t215, 0.10D1, z)
      t217 = s * t216
      t218 = -t214
      t219 = t1 * t218
      t220 = -t215
      t221 = t219 * t220
      t223 = t219 * x3
      t225 = t1 * x1
      t227 = t216 ** 2
      t230 = t218 * x1
      t234 = 0.1D1 / (-0.2D1 + t216)
      t235 = t23 * t234
      t237 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, t214, x2, t215, 0.
     #10D1)
      t242 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, t214, x2, t215, 0.
     #10D1)
      t245 = t218 ** 2
      t246 = t12 * t245
      t247 = t227 ** 2
      t252 = log(-0.4D1 * t177 * t10 * t246 * t220 * t247)
      t259 = t3 * t19
      t267 = -t63 * t235 * t237 * t92 * t173 / 0.8D1 - (0.90D2 * t63 * t
     #23 * (t234 * t242 - t252 * t234 * t237) - 0.180D3 * t259 * t235 * 
     #t237) * t92 * t167 / 0.720D3
      t268 = FJET(XB1, XB2, s, t217 * t221, -t217 * t223, t217 * t225, 0
     #.0D0, -s * t227 * t11 * t230 * x3, t267)
      t270 = KAPPA2(t214, x2, t215, t64, z)
      t271 = s * t270
      t274 = t225 * t69
      t276 = t225 * x4
      t278 = t270 ** 2
      t283 = cos(t5)
      t285 = x4 * t69
      t287 = Sqrt(x3 * t220 * t285)
      t294 = 0.1D1 / (-0.2D1 + t270)
      t297 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, t214, x2, t215, t6
     #4)
      t302 = FJET(XB1, XB2, s, t271 * t221, -t271 * t223, -t271 * t274, 
     #t271 * t276, s * t278 * t11 * t230 * (-x3 - x4 + 0.2D1 * x3 * x4 +
     # 0.2D1 * t283 * t287), t63 * t23 * t294 * t297 * t92 * t173 / 0.8D
     #1)
      t313 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, t214, x2, 0.10D1, 
     #0.10D1)
      t317 = log(0.4D1 * t151 * t246 * x4)
      t318 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, t214, x2, 0.10D1, 
     #0.10D1)
      t324 = t86 * t318
      t326 = 0.180D3 * t3 * t324
      t335 = t177 * t7
      t336 = t76 * t245
      t339 = log(0.4D1 * t335 * t336)
      t349 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, t214, x2, 0.10D1, 
     #0.10D1)
      t352 = log(0.4D1 * t150 * t336)
      t354 = t352 ** 2
      t370 = (0.90D2 * t63 * t23 * (-t313 + t317 * t318) + t326) * t167 
     #* t94 / 0.720D3 - t171 * t318 * t92 * t173 / 0.8D1 - (0.90D2 * t63
     # * t23 * (t313 - t339 * t318) - t326) * t92 * t167 / 0.720D3 + (-0
     #.90D2 * t63 * t23 * (t349 - t352 * t313 + t354 * t318 / 0.2D1) + 0
     #.180D3 * t3 * t86 * (t313 - t352 * t318) - t33 * t324) * t167 / 0.
     #720D3
      t371 = FJET(XB1, XB2, s, -t2 * t218, 0.0D0, t2 * x1, 0.0D0, 0.0D0,
     # t370)
      t375 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t215, 
     #t64)
      t376 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t215, 
     #0.10D1)
      t377 = -t375 + t376
      t382 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t215, 
     #0.10D1)
      t383 = t76 * t220
      t386 = log(-0.4D1 * t335 * t383)
      t392 = t86 * t376
      t399 = t12 * t220
      t403 = log(-0.4D1 * t67 * t399 * x4)
      t405 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t215, 
     #t64)
      t409 = log(0.4D1 * t67 * t399 * t285)
      t422 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t215, 
     #0.10D1)
      t425 = log(-0.4D1 * t66 * t383)
      t427 = t425 ** 2
      t443 = -t171 * t377 * t92 * t173 / 0.8D1 - (0.90D2 * t63 * t23 * (
     #t382 - t386 * t376) - 0.180D3 * t3 * t392) * t92 * t167 / 0.720D3 
     #- (0.90D2 * t63 * t23 * (t382 - t403 * t376 - t405 + t409 * t375) 
     #- 0.180D3 * t3 * t86 * t377) * t92 * t94 / 0.1440D4 + (-0.90D2 * t
     #63 * t23 * (t422 - t425 * t382 + t427 * t376 / 0.2D1) + 0.180D3 * 
     #t3 * t86 * (t382 - t425 * t376) - t33 * t392) * t92 / 0.1440D4
      t444 = FJET(XB1, XB2, s, -t2 * t220, t2 * x3, 0.0D0, 0.0D0, 0.0D0,
     # t443)
      t446 = KAPPA2(t214, x2, 0.10D1, t64, z)
      t447 = s * t446
      t451 = t446 ** 2
      t457 = 0.1D1 / (-0.2D1 + t446)
      t458 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, t214, x2, 0.10D1, 
     #t64)
      t461 = t451 ** 2
      t466 = log(-0.4D1 * t192 * t245 * x4 * t69 * t461)
      t468 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, t214, x2, 0.10D1, 
     #t64)
      t474 = t23 * t457
      t487 = (-0.90D2 * t63 * t23 * (t457 * t458 - t466 * t457 * t468) +
     # 0.180D3 * t259 * t474 * t468) * t167 * t94 / 0.720D3 - t63 * t474
     # * t468 * t92 * t173 / 0.8D1
      t488 = FJET(XB1, XB2, s, -t447 * t219, 0.0D0, -t447 * t274, t447 *
     # t276, -s * t451 * t11 * t230 * x4, t487)
      rrqq2qqht3s1e0 = t212 * t211 + t268 * t267 + t302 * 0.314159265358
     #9793D1 * t86 * t294 * t297 * t92 * t167 * t94 / 0.8D1 + t371 * t37
     #0 + t444 * t443 + t488 * t487

      end function



      doubleprecision function rrqq2qqht3s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1, 
     #0.10D1)
      t12 = 0.3141592653589793D1 * lh
      t15 = sin(x2 * 0.3141592653589793D1)
      t16 = t15 ** 2
      t17 = z ** 2
      t18 = 0.1D1 / t17
      t19 = t16 * t18
      t20 = t1 ** 2
      t21 = t20 ** 2
      t24 = log(0.4D1 * t19 * t21)
      t25 = t24 * 0.3141592653589793D1
      t29 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t33 = 0.3141592653589793D1 ** 2
      t35 = lh ** 2
      t41 = t24 ** 2
      t46 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t50 = t4 * t7
      t51 = 0.1D1 - x4
      t52 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # t51)
      t53 = t52 - t46
      t54 = 0.1D1 / x3
      t56 = 0.1D1 / x4
      t60 = x3 * t16
      t61 = t18 * t21
      t64 = log(0.4D1 * t60 * t61)
      t70 = t3 * t7
      t73 = 0.180D3 * t12 * t70 * t46
      t77 = t21 * x4
      t80 = log(0.4D1 * t19 * t77)
      t82 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # t51)
      t83 = -t51
      t87 = log(-0.4D1 * t19 * t77 * t83)
      t93 = -t53
      t101 = 0.1D1 / x1
      t105 = x1 ** 2
      t106 = t105 * t16
      t109 = log(0.4D1 * t106 * t61)
      t122 = t4 * t7 * t8 / 0.16D2 + (-0.180D3 * t12 - 0.90D2 * t25) * t
     #3 * t7 * t29 / 0.1440D4 + (0.3141592653589793D1 * (-0.30D2 * t33 +
     # 0.180D3 * t35) + 0.180D3 * t25 * lh + 0.45D2 * t41 * 0.3141592653
     #589793D1) * t3 * t7 * t46 / 0.1440D4 - t50 * t53 * t54 * t56 / 0.1
     #6D2 + (0.90D2 * t4 * t7 * (t29 - t64 * t46) - t73) * t54 / 0.1440D
     #4 + (0.90D2 * t4 * t7 * (t29 - t80 * t46 - t82 + t87 * t52) - 0.18
     #0D3 * t12 * t70 * t93) * t56 / 0.1440D4 + t50 * t46 * t54 * t101 /
     # 0.8D1 + (0.90D2 * t4 * t7 * (t29 - t109 * t46) - t73) * t101 / 0.
     #720D3 + t50 * t93 * t101 * t56 / 0.8D1
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
      t146 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, t125, x2, t126, 0.
     #10D1)
      t149 = 0.1D1 / (-0.2D1 + t127) * t146 * t54 * t101
      t152 = FJET(XB1, XB2, s, t128 * t130 * t131, -t128 * t130 * x3, t1
     #28 * t136, 0.0D0, -s * t138 * t20 * t141 * x3, -t50 * t149 / 0.8D1
     #)
      t159 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, t125, x2, 0.10D1, 
     #0.10D1)
      t164 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, t125, x2, 0.10D1, 
     #0.10D1)
      t165 = t129 ** 2
      t169 = log(0.4D1 * t106 * t61 * t165)
      t185 = -t50 * t159 * t54 * t101 / 0.8D1 + (-0.90D2 * t4 * t7 * (t1
     #64 - t169 * t159) + 0.180D3 * t12 * t70 * t159) * t101 / 0.720D3 -
     # t50 * t159 * t101 * t56 / 0.8D1
      t186 = FJET(XB1, XB2, s, -t2 * t129, 0.0D0, t2 * x1, 0.0D0, 0.0D0,
     # t185)
      t190 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t126, 
     #0.10D1)
      t195 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t126, 
     #t51)
      t201 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t126, 
     #0.10D1)
      t205 = log(-0.4D1 * t60 * t61 * t131)
      t217 = -t50 * t190 * t54 * t101 / 0.8D1 - t50 * (-t195 + t190) * t
     #54 * t56 / 0.16D2 + (-0.90D2 * t4 * t7 * (t201 - t205 * t190) + 0.
     #180D3 * t12 * t70 * t190) * t54 / 0.1440D4
      t218 = FJET(XB1, XB2, s, -t2 * t131, t2 * x3, 0.0D0, 0.0D0, 0.0D0,
     # t217)
      t220 = KAPPA2(t125, x2, 0.10D1, t51, z)
      t221 = s * t220
      t227 = t220 ** 2
      t234 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, t125, x2, 0.10D1, 
     #t51)
      t237 = 0.1D1 / (-0.2D1 + t220) * t234 * t101 * t56
      t240 = FJET(XB1, XB2, s, -t221 * t130, 0.0D0, -t221 * t136 * t83, 
     #t221 * t136 * x4, -s * t227 * t20 * t141 * x4, -t50 * t237 / 0.8D1
     #)
      rrqq2qqht3s1em1 = t123 * t122 - t152 * 0.3141592653589793D1 * t70 
     #* t149 / 0.8D1 + t186 * t185 + t218 * t217 - t240 * 0.314159265358
     #9793D1 * t70 * t237 / 0.8D1

      end function



      doubleprecision function rrqq2qqht3s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1, 
     #0.10D1)
      t10 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.1D1 - x4)
      t17 = t7 * t8
      t18 = 0.1D1 / x3
      t22 = 0.1D1 / x1
      t26 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.10D1)
      t33 = sin(x2 * 0.3141592653589793D1)
      t34 = t33 ** 2
      t35 = z ** 2
      t38 = t1 ** 2
      t39 = t38 ** 2
      t42 = log(0.4D1 * t34 / t35 * t39)
      t49 = t4 * t7 * (t8 - t10) / x4 / 0.16D2 + t4 * t17 * t18 / 0.16D2
     # + t4 * t17 * t22 / 0.8D1 + t4 * t7 * t26 / 0.16D2 + (-0.180D3 * 0
     #.3141592653589793D1 * lh - 0.90D2 * t42 * 0.3141592653589793D1) * 
     #t3 * t17 / 0.1440D4
      t50 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t49)
      t52 = -0.1D1 + x3
      t56 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, -t52, 0
     #.10D1)
      t58 = t7 * t56 * t18
      t61 = FJET(XB1, XB2, s, -t2 * t52, t2 * x3, 0.0D0, 0.0D0, 0.0D0, -
     #t4 * t58 / 0.16D2)
      t66 = -0.1D1 + x1
      t70 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, -t66, x2, 0.10D1, 0
     #.10D1)
      t72 = t7 * t70 * t22
      t75 = FJET(XB1, XB2, s, -t2 * t66, 0.0D0, t2 * x1, 0.0D0, 0.0D0, -
     #t4 * t72 / 0.8D1)
      rrqq2qqht3s1em2 = t50 * t49 - t61 * 0.3141592653589793D1 * t3 * t5
     #8 / 0.16D2 - t75 * 0.3141592653589793D1 * t3 * t72 / 0.8D1

      end function



      doubleprecision function rrqq2qqht3s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      t1 = z - 0.1D1
      t3 = 0.1D1 / t1
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1, 
     #0.10D1)
      t12 = FJET(XB1, XB2, s, s * t1, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.3141
     #592653589793D1 * t3 * t7 * t8 / 0.16D2)
      rrqq2qqht3s1em3 = t12 * 0.3141592653589793D1 * t3 * t7 * t8 / 0.16
     #D2

      end function



      doubleprecision function rrqq2qqht3s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      rrqq2qqht3s1em4 = 0.0D0

      end function


      doubleprecision function rrqq2qqht3s2e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.3141592653589793D1 ** 2
      t5 = lh ** 2
      t7 = -0.30D2 * t3 + 0.180D3 * t5
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
      t32 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
      t40 = -0.2884936567583026D3 - 0.120D3 * t5 * lh + 0.60D2 * lh * t3
      t41 = 0.3141592653589793D1 * t40
      t46 = t23 * t19 * 0.3141592653589793D1
      t50 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
      t54 = t3 ** 2
      t55 = t5 ** 2
      t67 = t23 ** 2
      t72 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
      t76 = 0.3141592653589793D1 * t27
      t77 = x1 ** 2
      t78 = t77 * t11
      t79 = t13 * t16
      t80 = t79 * x4
      t83 = log(0.4D1 * t78 * t80)
      t85 = t83 ** 2
      t88 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # x4)
      t89 = t78 * t13
      t90 = t16 * x4
      t91 = -0.1D1 + x4
      t92 = t90 * t91
      t95 = log(-0.4D1 * t89 * t92)
      t96 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # x4)
      t98 = t95 ** 2
      t99 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # x4)
      t106 = 0.3141592653589793D1 * lh
      t107 = t27 * t31
      t114 = -t72 + t99
      t116 = t8 * t107 * t114
      t118 = 0.1D1 / x1
      t120 = 0.1D1 / x4
      t123 = t78 * t79
      t125 = log(0.4D1 * t123)
      t131 = t125 ** 2
      t141 = t107 * t72
      t142 = t41 * t141
      t153 = x3 * t77
      t154 = t153 * t11
      t157 = log(0.4D1 * t154 * t80)
      t159 = x4 * t91
      t163 = log(-0.4D1 * t154 * t79 * t159)
      t170 = -t107 * t114
      t174 = 0.1D1 / x3
      t176 = t118 * t120
      t179 = t153 * t17
      t181 = log(0.4D1 * t179)
      t183 = t181 ** 2
      t202 = log(0.4D1 * t14 * t90)
      t206 = log(-0.4D1 * t14 * t92)
      t212 = t206 ** 2
      t219 = t202 ** 2
      t243 = x3 * t11
      t246 = log(0.4D1 * t243 * t80)
      t248 = t246 ** 2
      t251 = t243 * t13
      t254 = log(-0.4D1 * t251 * t92)
      t256 = t254 ** 2
      t275 = log(0.4D1 * t243 * t79)
      t281 = t275 ** 2
      t301 = (t8 + 0.180D3 * t20 * lh + 0.45D2 * t24) * t27 * t31 * t32 
     #/ 0.1440D4 + (t41 - t20 * t7 - 0.90D2 * t24 * lh - 0.15D2 * t46) *
     # t27 * t31 * t50 / 0.1440D4 + (0.3141592653589793D1 * (t54 + 0.60D
     #2 * t55 + 0.5769873135166051D3 * lh - 0.60D2 * t5 * t3) - t20 * t4
     #0 + t24 * t7 / 0.2D1 + 0.30D2 * t46 * lh + 0.15D2 / 0.4D1 * t67 * 
     #0.3141592653589793D1) * t27 * t31 * t72 / 0.1440D4 - (0.90D2 * t76
     # * t31 * (-t32 + t83 * t50 - t85 * t72 / 0.2D1 + t88 - t95 * t96 +
     # t98 * t99 / 0.2D1) - 0.180D3 * t106 * t107 * (-t50 + t83 * t72 + 
     #t96 - t95 * t99) + t116) * t118 * t120 / 0.720D3 + (t8 * t107 * (t
     #50 - t125 * t72) + 0.90D2 * t76 * t31 * (-t125 * t32 + t131 * t50 
     #/ 0.2D1 - t131 * t125 * t72 / 0.6D1) + t142 - 0.180D3 * t106 * t10
     #7 * (t32 - t125 * t50 + t131 * t72 / 0.2D1)) * t118 / 0.720D3 + (0
     #.90D2 * t76 * t31 * (t50 - t157 * t72 - t96 + t163 * t99) - 0.180D
     #3 * t106 * t170) * t174 * t176 / 0.720D3 + (0.90D2 * t76 * t31 * (
     #t32 - t181 * t50 + t183 * t72 / 0.2D1) - 0.180D3 * t106 * t107 * (
     #t50 - t181 * t72) + t8 * t141) * t174 * t118 / 0.720D3 + (t8 * t10
     #7 * (t50 - t202 * t72 - t96 + t206 * t99) + 0.90D2 * t76 * t31 * (
     #t206 * t88 - t212 * t96 / 0.2D1 + t212 * t206 * t99 / 0.6D1 - t202
     # * t32 + t219 * t50 / 0.2D1 - t219 * t202 * t72 / 0.6D1) + t41 * t
     #170 - 0.180D3 * t106 * t107 * (t32 - t202 * t50 + t219 * t72 / 0.2
     #D1 - t88 + t206 * t96 - t212 * t99 / 0.2D1)) * t120 / 0.1440D4 - (
     #0.90D2 * t76 * t31 * (-t32 + t246 * t50 - t248 * t72 / 0.2D1 + t88
     # - t254 * t96 + t256 * t99 / 0.2D1) - 0.180D3 * t106 * t107 * (-t5
     #0 + t246 * t72 + t96 - t254 * t99) + t116) * t174 * t120 / 0.1440D
     #4 - (t8 * t107 * (-t50 + t275 * t72) + 0.90D2 * t76 * t31 * (t275 
     #* t32 - t281 * t50 / 0.2D1 + t281 * t275 * t72 / 0.6D1) - t142 - 0
     #.180D3 * t106 * t107 * (-t32 + t275 * t50 - t281 * t72 / 0.2D1)) *
     # t174 / 0.1440D4
      t302 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t301)
      t304 = 0.1D1 - x1
      t305 = 0.1D1 - x3
      t306 = KAPPA2(t304, x2, t305, 0.0D0, z)
      t307 = s * t306
      t308 = -t304
      t309 = t1 * t308
      t310 = -t305
      t311 = t309 * t310
      t313 = t309 * x3
      t315 = t1 * x1
      t317 = t306 ** 2
      t320 = t308 * x1
      t324 = 0.1D1 / (-0.2D1 + t306)
      t325 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, t304, x2, t305, 0.
     #0D0)
      t326 = t324 * t325
      t327 = t153 * t14
      t328 = t308 ** 2
      t329 = t16 * t328
      t330 = t310 * x4
      t331 = t317 ** 2
      t336 = log(-0.4D1 * t327 * t329 * t330 * t331)
      t338 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, t304, x2, t305, 0.
     #0D0)
      t344 = t106 * t27
      t346 = t31 * t324 * t338
      t352 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, t304, x2, t305, 0.
     #0D0)
      t358 = log(-0.4D1 * t327 * t329 * t310 * t331)
      t359 = t358 * t324
      t361 = t358 ** 2
      t374 = t8 * t27
      t380 = (0.90D2 * t76 * t31 * (-t326 + t336 * t324 * t338) + 0.180D
     #3 * t344 * t346) * t174 * t176 / 0.720D3 + (0.90D2 * t76 * t31 * (
     #-t324 * t352 + t359 * t325 - t361 * t324 * t338 / 0.2D1) - 0.180D3
     # * t106 * t107 * (-t326 + t359 * t338) - t374 * t346) * t174 * t11
     #8 / 0.720D3
      t381 = FJET(XB1, XB2, s, t307 * t311, -t307 * t313, 0.0D0, t307 * 
     #t315, s * t317 * t15 * t320 * t310, t380)
      t383 = KAPPA2(t304, x2, t305, x4, z)
      t384 = s * t383
      t387 = t315 * x4
      t389 = t315 * t91
      t391 = t383 ** 2
      t396 = cos(t9)
      t399 = Sqrt(x3 * t310 * t159)
      t406 = 0.1D1 / (-0.2D1 + t383)
      t407 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, t304, x2, t305, x4
     #)
      t410 = t391 ** 2
      t415 = log(0.4D1 * t179 * t328 * t310 * t159 * t410)
      t417 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, t304, x2, t305, x4
     #)
      t427 = 0.90D2 * t76 * t31 * (t406 * t407 - t415 * t406 * t417) - 0
     #.180D3 * t344 * t31 * t406 * t417
      t431 = FJET(XB1, XB2, s, t384 * t311, -t384 * t313, t384 * t387, -
     #t384 * t389, s * t391 * t15 * t320 * (-0.1D1 + x3 + x4 - 0.2D1 * x
     #3 * x4 + 0.2D1 * t396 * t399), t427 * t174 * t176 / 0.720D3)
      t439 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t305, 
     #x4)
      t440 = t16 * t310
      t441 = t440 * t159
      t444 = log(0.4D1 * t251 * t441)
      t445 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t305, 
     #x4)
      t447 = t444 ** 2
      t448 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t305, 
     #x4)
      t451 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t305, 
     #0.0D0)
      t455 = log(-0.4D1 * t251 * t440 * x4)
      t456 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t305, 
     #0.0D0)
      t458 = t455 ** 2
      t459 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t305, 
     #0.0D0)
      t472 = -t448 + t459
      t479 = t79 * t310
      t482 = log(-0.4D1 * t243 * t479)
      t488 = t482 ** 2
      t498 = t107 * t459
      t513 = log(-0.4D1 * t154 * t79 * t330)
      t517 = log(0.4D1 * t327 * t441)
      t533 = log(-0.4D1 * t154 * t479)
      t535 = t533 ** 2
      t552 = -(0.90D2 * t76 * t31 * (-t439 + t444 * t445 - t447 * t448 /
     # 0.2D1 + t451 - t455 * t456 + t458 * t459 / 0.2D1) - 0.180D3 * t10
     #6 * t107 * (-t445 + t444 * t448 + t456 - t455 * t459) + t8 * t107 
     #* t472) * t174 * t120 / 0.1440D4 - (t8 * t107 * (t456 - t482 * t45
     #9) + 0.90D2 * t76 * t31 * (-t482 * t451 + t488 * t456 / 0.2D1 - t4
     #88 * t482 * t459 / 0.6D1) + t41 * t498 - 0.180D3 * t106 * t107 * (
     #t451 - t482 * t456 + t488 * t459 / 0.2D1)) * t174 / 0.1440D4 + (0.
     #90D2 * t76 * t31 * (-t456 + t513 * t459 + t445 - t517 * t448) + 0.
     #180D3 * t106 * t107 * t472) * t174 * t176 / 0.720D3 + (0.90D2 * t7
     #6 * t31 * (-t451 + t533 * t456 - t535 * t459 / 0.2D1) - 0.180D3 * 
     #t106 * t107 * (-t456 + t533 * t459) - t8 * t498) * t174 * t118 / 0
     #.720D3
      t553 = FJET(XB1, XB2, s, -t2 * t310, t2 * x3, 0.0D0, 0.0D0, 0.0D0,
     # t552)
      t555 = KAPPA2(t304, x2, 0.10D1, 0.0D0, z)
      t556 = s * t555
      t559 = t555 ** 2
      t565 = 0.1D1 / (-0.2D1 + t555)
      t566 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, t304, x2, 0.10D1, 
     #0.0D0)
      t567 = t565 * t566
      t568 = t559 ** 2
      t570 = t329 * x4 * t568
      t573 = log(0.4D1 * t89 * t570)
      t574 = t573 * t565
      t575 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, t304, x2, 0.10D1, 
     #0.0D0)
      t577 = t573 ** 2
      t579 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, t304, x2, 0.10D1, 
     #0.0D0)
      t586 = t565 * t575
      t593 = t31 * t565 * t579
      t594 = t374 * t593
      t601 = log(0.4D1 * t89 * t329 * t568)
      t602 = t601 * t565
      t608 = t601 ** 2
      t609 = t608 * t565
      t633 = log(0.4D1 * t327 * t570)
      t649 = log(0.4D1 * t154 * t79 * t328 * t568)
      t650 = t649 * t565
      t652 = t649 ** 2
      t669 = -(0.90D2 * t76 * t31 * (-t567 + t574 * t575 - t577 * t565 *
     # t579 / 0.2D1) - 0.180D3 * t106 * t107 * (-t586 + t574 * t579) - t
     #594) * t118 * t120 / 0.720D3 + (t8 * t107 * (t586 - t602 * t579) +
     # 0.90D2 * t76 * t31 * (-t602 * t566 + t609 * t575 / 0.2D1 - t608 *
     # t601 * t565 * t579 / 0.6D1) + t41 * t27 * t593 - 0.180D3 * t106 *
     # t107 * (t567 - t602 * t575 + t609 * t579 / 0.2D1)) * t118 / 0.720
     #D3 + (0.90D2 * t76 * t31 * (t586 - t633 * t565 * t579) - 0.180D3 *
     # t344 * t593) * t174 * t176 / 0.720D3 + (0.90D2 * t76 * t31 * (t56
     #7 - t650 * t575 + t652 * t565 * t579 / 0.2D1) - 0.180D3 * t106 * t
     #107 * (t586 - t650 * t579) + t594) * t174 * t118 / 0.720D3
      t670 = FJET(XB1, XB2, s, -t556 * t309, 0.0D0, 0.0D0, t556 * t315, 
     #-s * t559 * t15 * t308 * x1, t669)
      t672 = KAPPA2(t304, x2, 0.10D1, x4, z)
      t673 = s * t672
      t677 = t672 ** 2
      t683 = 0.1D1 / (-0.2D1 + t672)
      t684 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, t304, x2, 0.10D1, 
     #x4)
      t687 = t677 ** 2
      t692 = log(-0.4D1 * t123 * t328 * x4 * t91 * t687)
      t693 = t692 * t683
      t694 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, t304, x2, 0.10D1, 
     #x4)
      t696 = t692 ** 2
      t698 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, t304, x2, 0.10D1, 
     #x4)
      t705 = t683 * t694
      t712 = t31 * t683 * t698
      t721 = log(-0.4D1 * t327 * t329 * t159 * t687)
      t734 = -(0.90D2 * t76 * t31 * (t683 * t684 - t693 * t694 + t696 * 
     #t683 * t698 / 0.2D1) - 0.180D3 * t106 * t107 * (t705 - t693 * t698
     #) + t374 * t712) * t118 * t120 / 0.720D3 + (-0.90D2 * t76 * t31 * 
     #(t705 - t721 * t683 * t698) + 0.180D3 * t344 * t712) * t174 * t176
     # / 0.720D3
      t735 = FJET(XB1, XB2, s, -t673 * t309, 0.0D0, t673 * t387, -t673 *
     # t389, s * t677 * t15 * t320 * t91, t734)
      rrqq2qqht3s2e1 = t302 * t301 + t381 * t380 + t431 * t427 * t174 * 
     #t118 * t120 / 0.720D3 + t553 * t552 + t669 * t670 + t735 * t734

      end function



      doubleprecision function rrqq2qqht3s2e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.3141592653589793D1 * lh
      t5 = x2 * 0.3141592653589793D1
      t6 = sin(t5)
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
      t24 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
      t28 = 0.3141592653589793D1 ** 2
      t30 = lh ** 2
      t32 = -0.30D2 * t28 + 0.180D3 * t30
      t33 = 0.3141592653589793D1 * t32
      t36 = t15 ** 2
      t37 = t36 * 0.3141592653589793D1
      t41 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
      t59 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
      t63 = t19 * 0.3141592653589793D1
      t64 = x3 * t7
      t65 = t9 * t12
      t66 = t65 * x4
      t69 = log(0.4D1 * t64 * t66)
      t71 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # x4)
      t72 = t64 * t9
      t73 = t12 * x4
      t74 = -0.1D1 + x4
      t75 = t73 * t74
      t78 = log(-0.4D1 * t72 * t75)
      t79 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # x4)
      t85 = t19 * t23
      t86 = -t59 + t79
      t89 = 0.180D3 * t3 * t85 * t86
      t91 = 0.1D1 / x3
      t93 = 0.1D1 / x4
      t98 = log(0.4D1 * t64 * t65)
      t100 = t98 ** 2
      t112 = t85 * t59
      t113 = t33 * t112
      t119 = log(0.4D1 * t10 * t73)
      t121 = t119 ** 2
      t124 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t127 = log(-0.4D1 * t10 * t75)
      t129 = t127 ** 2
      t142 = -t86
      t148 = x1 ** 2
      t149 = t148 * t7
      t152 = log(0.4D1 * t149 * t66)
      t154 = t149 * t9
      t157 = log(-0.4D1 * t154 * t75)
      t164 = 0.1D1 / x1
      t168 = t63 * t23
      t170 = t164 * t93
      t174 = x3 * t148
      t177 = log(0.4D1 * t174 * t13)
      t189 = t149 * t65
      t191 = log(0.4D1 * t189)
      t193 = t191 ** 2
      t208 = (-0.180D3 * t3 - 0.90D2 * t16) * t19 * t23 * t24 / 0.1440D4
     # + (t33 + 0.180D3 * t16 * lh + 0.45D2 * t37) * t19 * t23 * t41 / 0
     #.1440D4 + (0.3141592653589793D1 * (-0.2884936567583026D3 - 0.120D3
     # * t30 * lh + 0.60D2 * lh * t28) - t16 * t32 - 0.90D2 * t37 * lh -
     # 0.15D2 * t36 * t15 * 0.3141592653589793D1) * t19 * t23 * t59 / 0.
     #1440D4 - (0.90D2 * t63 * t23 * (-t41 + t69 * t59 + t71 - t78 * t79
     #) - t89) * t91 * t93 / 0.1440D4 - (0.90D2 * t63 * t23 * (-t24 + t9
     #8 * t41 - t100 * t59 / 0.2D1) - 0.180D3 * t3 * t85 * (-t41 + t98 *
     # t59) - t113) * t91 / 0.1440D4 + (0.90D2 * t63 * t23 * (t24 - t119
     # * t41 + t121 * t59 / 0.2D1 - t124 + t127 * t71 - t129 * t79 / 0.2
     #D1) - 0.180D3 * t3 * t85 * (t41 - t119 * t59 - t71 + t127 * t79) +
     # t33 * t85 * t142) * t93 / 0.1440D4 - (0.90D2 * t63 * t23 * (-t41 
     #+ t152 * t59 + t71 - t157 * t79) - t89) * t164 * t93 / 0.720D3 + t
     #168 * t142 * t91 * t170 / 0.8D1 + (0.90D2 * t63 * t23 * (t41 - t17
     #7 * t59) - 0.180D3 * t3 * t112) * t91 * t164 / 0.720D3 + (0.90D2 *
     # t63 * t23 * (t24 - t191 * t41 + t193 * t59 / 0.2D1) - 0.180D3 * t
     #3 * t85 * (t41 - t191 * t59) + t113) * t164 / 0.720D3
      t209 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t208)
      t211 = 0.1D1 - x1
      t212 = 0.1D1 - x3
      t213 = KAPPA2(t211, x2, t212, 0.0D0, z)
      t214 = s * t213
      t215 = -t211
      t216 = t1 * t215
      t217 = -t212
      t218 = t216 * t217
      t220 = t216 * x3
      t222 = t1 * x1
      t224 = t213 ** 2
      t227 = t215 * x1
      t231 = 0.1D1 / (-0.2D1 + t213)
      t232 = t23 * t231
      t234 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, t211, x2, t212, 0.
     #0D0)
      t239 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, t211, x2, t212, 0.
     #0D0)
      t242 = t215 ** 2
      t243 = t12 * t242
      t244 = t224 ** 2
      t249 = log(-0.4D1 * t174 * t10 * t243 * t217 * t244)
      t256 = t3 * t19
      t264 = -t63 * t232 * t234 * t91 * t170 / 0.8D1 + (0.90D2 * t63 * t
     #23 * (-t231 * t239 + t249 * t231 * t234) + 0.180D3 * t256 * t232 *
     # t234) * t91 * t164 / 0.720D3
      t265 = FJET(XB1, XB2, s, t214 * t218, -t214 * t220, 0.0D0, t214 * 
     #t222, s * t224 * t11 * t227 * t217, t264)
      t267 = KAPPA2(t211, x2, t212, x4, z)
      t268 = s * t267
      t271 = t222 * x4
      t273 = t222 * t74
      t275 = t267 ** 2
      t280 = cos(t5)
      t282 = x4 * t74
      t284 = Sqrt(x3 * t217 * t282)
      t291 = 0.1D1 / (-0.2D1 + t267)
      t294 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, t211, x2, t212, x4
     #)
      t299 = FJET(XB1, XB2, s, t268 * t218, -t268 * t220, t268 * t271, -
     #t268 * t273, s * t275 * t11 * t227 * (-0.1D1 + x3 + x4 - 0.2D1 * x
     #3 * x4 + 0.2D1 * t280 * t284), t63 * t23 * t291 * t294 * t91 * t17
     #0 / 0.8D1)
      t310 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t212, 
     #x4)
      t311 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t212, 
     #0.0D0)
      t312 = t310 - t311
      t317 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t212, 
     #0.0D0)
      t318 = t174 * t7
      t319 = t65 * t217
      t322 = log(-0.4D1 * t318 * t319)
      t328 = t85 * t311
      t335 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t212, 
     #x4)
      t336 = t12 * t217
      t340 = log(0.4D1 * t72 * t336 * t282)
      t345 = log(-0.4D1 * t72 * t336 * x4)
      t359 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t212, 
     #0.0D0)
      t362 = log(-0.4D1 * t64 * t319)
      t364 = t362 ** 2
      t380 = t168 * t312 * t91 * t170 / 0.8D1 + (0.90D2 * t63 * t23 * (-
     #t317 + t322 * t311) + 0.180D3 * t3 * t328) * t91 * t164 / 0.720D3 
     #- (0.90D2 * t63 * t23 * (-t335 + t340 * t310 + t317 - t345 * t311)
     # + 0.180D3 * t3 * t85 * t312) * t91 * t93 / 0.1440D4 - (0.90D2 * t
     #63 * t23 * (t359 - t362 * t317 + t364 * t311 / 0.2D1) - 0.180D3 * 
     #t3 * t85 * (t317 - t362 * t311) + t33 * t328) * t91 / 0.1440D4
      t381 = FJET(XB1, XB2, s, -t2 * t217, t2 * x3, 0.0D0, 0.0D0, 0.0D0,
     # t380)
      t383 = KAPPA2(t211, x2, 0.10D1, 0.0D0, z)
      t384 = s * t383
      t387 = t383 ** 2
      t393 = 0.1D1 / (-0.2D1 + t383)
      t394 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, t211, x2, 0.10D1, 
     #0.0D0)
      t395 = t393 * t394
      t396 = t387 ** 2
      t401 = log(0.4D1 * t154 * t243 * x4 * t396)
      t403 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, t211, x2, 0.10D1, 
     #0.0D0)
      t409 = t23 * t393
      t410 = t409 * t403
      t412 = 0.180D3 * t256 * t410
      t426 = log(0.4D1 * t318 * t65 * t242 * t396)
      t437 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, t211, x2, 0.10D1, 
     #0.0D0)
      t442 = log(0.4D1 * t154 * t243 * t396)
      t443 = t442 * t393
      t445 = t442 ** 2
      t463 = -(0.90D2 * t63 * t23 * (-t395 + t401 * t393 * t403) + t412)
     # * t164 * t93 / 0.720D3 + t63 * t409 * t403 * t91 * t170 / 0.8D1 +
     # (0.90D2 * t63 * t23 * (t395 - t426 * t393 * t403) - t412) * t91 *
     # t164 / 0.720D3 + (0.90D2 * t63 * t23 * (t393 * t437 - t443 * t394
     # + t445 * t393 * t403 / 0.2D1) - 0.180D3 * t3 * t85 * (t395 - t443
     # * t403) + t33 * t19 * t410) * t164 / 0.720D3
      t464 = FJET(XB1, XB2, s, -t384 * t216, 0.0D0, 0.0D0, t384 * t222, 
     #-s * t387 * t11 * t215 * x1, t463)
      t466 = KAPPA2(t211, x2, 0.10D1, x4, z)
      t467 = s * t466
      t471 = t466 ** 2
      t477 = 0.1D1 / (-0.2D1 + t466)
      t478 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, t211, x2, 0.10D1, 
     #x4)
      t481 = t471 ** 2
      t486 = log(-0.4D1 * t189 * t242 * x4 * t74 * t481)
      t488 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, t211, x2, 0.10D1, 
     #x4)
      t494 = t23 * t477
      t507 = -(0.90D2 * t63 * t23 * (t477 * t478 - t486 * t477 * t488) -
     # 0.180D3 * t256 * t494 * t488) * t164 * t93 / 0.720D3 - t63 * t494
     # * t488 * t91 * t170 / 0.8D1
      t508 = FJET(XB1, XB2, s, -t467 * t216, 0.0D0, t467 * t271, -t467 *
     # t273, s * t471 * t11 * t227 * t74, t507)
      rrqq2qqht3s2e0 = t209 * t208 + t265 * t264 + t299 * 0.314159265358
     #9793D1 * t85 * t291 * t294 * t91 * t164 * t93 / 0.8D1 + t381 * t38
     #0 + t464 * t463 + t508 * t507

      end function



      doubleprecision function rrqq2qqht3s2em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1, 
     #0.0D0)
      t12 = 0.3141592653589793D1 * lh
      t15 = sin(x2 * 0.3141592653589793D1)
      t16 = t15 ** 2
      t17 = z ** 2
      t18 = 0.1D1 / t17
      t19 = t16 * t18
      t20 = t1 ** 2
      t21 = t20 ** 2
      t24 = log(0.4D1 * t19 * t21)
      t25 = t24 * 0.3141592653589793D1
      t29 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
      t33 = 0.3141592653589793D1 ** 2
      t35 = lh ** 2
      t41 = t24 ** 2
      t46 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # 0.0D0)
      t50 = t4 * t7
      t51 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # x4)
      t52 = -t46 + t51
      t53 = 0.1D1 / x3
      t55 = 0.1D1 / x4
      t59 = x3 * t16
      t60 = t18 * t21
      t63 = log(0.4D1 * t59 * t60)
      t69 = t3 * t7
      t72 = 0.180D3 * t12 * t69 * t46
      t76 = t21 * x4
      t79 = log(0.4D1 * t19 * t76)
      t81 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
     # x4)
      t82 = -0.1D1 + x4
      t86 = log(-0.4D1 * t19 * t76 * t82)
      t100 = 0.1D1 / x1
      t104 = x1 ** 2
      t105 = t104 * t16
      t108 = log(0.4D1 * t105 * t60)
      t121 = t4 * t7 * t8 / 0.16D2 + (-0.180D3 * t12 - 0.90D2 * t25) * t
     #3 * t7 * t29 / 0.1440D4 + (0.3141592653589793D1 * (-0.30D2 * t33 +
     # 0.180D3 * t35) + 0.180D3 * t25 * lh + 0.45D2 * t41 * 0.3141592653
     #589793D1) * t3 * t7 * t46 / 0.1440D4 - t50 * t52 * t53 * t55 / 0.1
     #6D2 - (0.90D2 * t4 * t7 * (-t29 + t63 * t46) + t72) * t53 / 0.1440
     #D4 + (0.90D2 * t4 * t7 * (t29 - t79 * t46 - t81 + t86 * t51) + 0.1
     #80D3 * t12 * t69 * t52) * t55 / 0.1440D4 + t50 * t46 * t53 * t100 
     #/ 0.8D1 + (0.90D2 * t4 * t7 * (t29 - t108 * t46) - t72) * t100 / 0
     #.720D3 - t50 * t52 * t100 * t55 / 0.8D1
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
      t145 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, t124, x2, t125, 0.
     #0D0)
      t147 = t53 * t100
      t148 = 0.1D1 / (-0.2D1 + t126) * t145 * t147
      t151 = FJET(XB1, XB2, s, t127 * t129 * t130, -t127 * t129 * x3, 0.
     #0D0, t127 * t135, s * t137 * t20 * t140 * t130, -t50 * t148 / 0.8D
     #1)
      t158 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t125, 
     #0.0D0)
      t163 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t125, 
     #x4)
      t169 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t125, 
     #0.0D0)
      t173 = log(-0.4D1 * t59 * t60 * t130)
      t185 = -t50 * t158 * t53 * t100 / 0.8D1 - t50 * (-t163 + t158) * t
     #53 * t55 / 0.16D2 - (0.90D2 * t4 * t7 * (t169 - t173 * t158) - 0.1
     #80D3 * t12 * t69 * t158) * t53 / 0.1440D4
      t186 = FJET(XB1, XB2, s, -t2 * t130, t2 * x3, 0.0D0, 0.0D0, 0.0D0,
     # t185)
      t188 = KAPPA2(t124, x2, 0.10D1, 0.0D0, z)
      t189 = s * t188
      t192 = t188 ** 2
      t198 = 0.1D1 / (-0.2D1 + t188)
      t199 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, t124, x2, 0.10D1, 
     #0.0D0)
      t200 = t198 * t199
      t204 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, t124, x2, 0.10D1, 
     #0.0D0)
      t207 = t128 ** 2
      t209 = t192 ** 2
      t213 = log(0.4D1 * t105 * t18 * t21 * t207 * t209)
      t228 = t100 * t55
      t232 = t50 * t200 * t147 / 0.8D1 + (0.90D2 * t4 * t7 * (t198 * t20
     #4 - t213 * t198 * t199) - 0.180D3 * t12 * t3 * t7 * t198 * t199) *
     # t100 / 0.720D3 + t50 * t200 * t228 / 0.8D1
      t233 = FJET(XB1, XB2, s, -t189 * t129, 0.0D0, 0.0D0, t189 * t135, 
     #-s * t192 * t20 * t128 * x1, t232)
      t235 = KAPPA2(t124, x2, 0.10D1, x4, z)
      t236 = s * t235
      t242 = t235 ** 2
      t249 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, t124, x2, 0.10D1, 
     #x4)
      t251 = 0.1D1 / (-0.2D1 + t235) * t249 * t228
      t254 = FJET(XB1, XB2, s, -t236 * t129, 0.0D0, t236 * t135 * x4, -t
     #236 * t135 * t82, s * t242 * t20 * t140 * t82, -t50 * t251 / 0.8D1
     #)
      rrqq2qqht3s2em1 = t122 * t121 - t151 * 0.3141592653589793D1 * t69 
     #* t148 / 0.8D1 + t186 * t185 + t233 * t232 - t254 * 0.314159265358
     #9793D1 * t69 * t251 / 0.8D1

      end function



      doubleprecision function rrqq2qqht3s2em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1, 
     #0.0D0)
      t9 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1, 
     #x4)
      t16 = t7 * t8
      t17 = 0.1D1 / x3
      t21 = 0.1D1 / x1
      t25 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1,
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
      t55 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, -t51, 0
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
      t81 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, t65, x2, 0.10D1, 0.
     #0D0)
      t86 = FJET(XB1, XB2, s, -t67 * t1 * t68, 0.0D0, 0.0D0, t67 * t1 * 
     #x1, -s * t73 * t37 * t68 * x1, t4 * t7 * t80 * t81 * t21 / 0.8D1)
      rrqq2qqht3s2em2 = t49 * t48 - t60 * 0.3141592653589793D1 * t3 * t5
     #7 / 0.16D2 + t86 * 0.3141592653589793D1 * t3 * t7 * t80 * t81 * t2
     #1 / 0.8D1

      end function



      doubleprecision function rrqq2qqht3s2em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      t1 = z - 0.1D1
      t3 = 0.1D1 / t1
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1, 
     #0.0D0)
      t12 = FJET(XB1, XB2, s, s * t1, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.3141
     #592653589793D1 * t3 * t7 * t8 / 0.16D2)
      rrqq2qqht3s2em3 = t12 * 0.3141592653589793D1 * t3 * t7 * t8 / 0.16
     #D2

      end function



      doubleprecision function rrqq2qqht3s2em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      rrqq2qqht3s2em4 = 0.0D0

      end function


      doubleprecision function rrqq2qqht3s3e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.3141592653589793D1 ** 2
      t5 = lh ** 2
      t7 = -0.30D2 * t3 + 0.180D3 * t5
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
      t32 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t40 = -0.2884936567583026D3 - 0.120D3 * t5 * lh + 0.60D2 * lh * t3
      t41 = 0.3141592653589793D1 * t40
      t46 = t23 * t19 * 0.3141592653589793D1
      t50 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t54 = t3 ** 2
      t55 = t5 ** 2
      t67 = t23 ** 2
      t72 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t76 = 0.3141592653589793D1 * t27
      t77 = 0.1D1 - x4
      t78 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #t77)
      t79 = x1 ** 2
      t80 = t79 * t11
      t81 = t80 * t13
      t82 = t16 * x4
      t83 = -t77
      t84 = t82 * t83
      t87 = log(-0.4D1 * t81 * t84)
      t88 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #t77)
      t90 = t87 ** 2
      t91 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #t77)
      t94 = t13 * t16
      t95 = t94 * x4
      t98 = log(0.4D1 * t80 * t95)
      t100 = t98 ** 2
      t107 = 0.3141592653589793D1 * lh
      t108 = t27 * t31
      t115 = -t91 + t72
      t116 = t108 * t115
      t119 = 0.1D1 / x1
      t121 = 0.1D1 / x4
      t124 = t80 * t94
      t126 = log(0.4D1 * t124)
      t132 = t126 ** 2
      t142 = t108 * t72
      t143 = t41 * t142
      t154 = x3 * t79
      t155 = t154 * t11
      t158 = log(0.4D1 * t155 * t95)
      t160 = x4 * t83
      t164 = log(-0.4D1 * t155 * t94 * t160)
      t171 = -t108 * t115
      t175 = 0.1D1 / x3
      t177 = t119 * t121
      t180 = t154 * t17
      t182 = log(0.4D1 * t180)
      t184 = t182 ** 2
      t203 = log(-0.4D1 * t14 * t84)
      t207 = log(0.4D1 * t14 * t82)
      t213 = t207 ** 2
      t220 = t203 ** 2
      t244 = x3 * t11
      t245 = t244 * t13
      t248 = log(-0.4D1 * t245 * t84)
      t250 = t248 ** 2
      t255 = log(0.4D1 * t244 * t95)
      t257 = t255 ** 2
      t277 = log(0.4D1 * t244 * t94)
      t283 = t277 ** 2
      t303 = (t8 + 0.180D3 * t20 * lh + 0.45D2 * t24) * t27 * t31 * t32 
     #/ 0.1440D4 + (t41 - t20 * t7 - 0.90D2 * t24 * lh - 0.15D2 * t46) *
     # t27 * t31 * t50 / 0.1440D4 + (0.3141592653589793D1 * (t54 + 0.60D
     #2 * t55 + 0.5769873135166051D3 * lh - 0.60D2 * t5 * t3) - t20 * t4
     #0 + t24 * t7 / 0.2D1 + 0.30D2 * t46 * lh + 0.15D2 / 0.4D1 * t67 * 
     #0.3141592653589793D1) * t27 * t31 * t72 / 0.1440D4 + (0.90D2 * t76
     # * t31 * (-t78 + t87 * t88 - t90 * t91 / 0.2D1 + t32 - t98 * t50 +
     # t100 * t72 / 0.2D1) - 0.180D3 * t107 * t108 * (-t88 + t87 * t91 +
     # t50 - t98 * t72) + t8 * t116) * t119 * t121 / 0.720D3 - (t8 * t10
     #8 * (-t50 + t126 * t72) + 0.90D2 * t76 * t31 * (t126 * t32 - t132 
     #* t50 / 0.2D1 + t132 * t126 * t72 / 0.6D1) - t143 - 0.180D3 * t107
     # * t108 * (-t32 + t126 * t50 - t132 * t72 / 0.2D1)) * t119 / 0.720
     #D3 - (0.90D2 * t76 * t31 * (-t50 + t158 * t72 + t88 - t164 * t91) 
     #- 0.180D3 * t107 * t171) * t175 * t177 / 0.720D3 - (0.90D2 * t76 *
     # t31 * (-t32 + t182 * t50 - t184 * t72 / 0.2D1) - 0.180D3 * t107 *
     # t108 * (-t50 + t182 * t72) - t8 * t142) * t175 * t119 / 0.720D3 +
     # (t8 * t108 * (-t88 + t203 * t91 + t50 - t207 * t72) + 0.90D2 * t7
     #6 * t31 * (-t207 * t32 + t213 * t50 / 0.2D1 - t213 * t207 * t72 / 
     #0.6D1 + t203 * t78 - t220 * t88 / 0.2D1 + t220 * t203 * t91 / 0.6D
     #1) + t41 * t116 - 0.180D3 * t107 * t108 * (-t78 + t203 * t88 - t22
     #0 * t91 / 0.2D1 + t32 - t207 * t50 + t213 * t72 / 0.2D1)) * t121 /
     # 0.1440D4 - (0.90D2 * t76 * t31 * (t78 - t248 * t88 + t250 * t91 /
     # 0.2D1 - t32 + t255 * t50 - t257 * t72 / 0.2D1) - 0.180D3 * t107 *
     # t108 * (t88 - t248 * t91 - t50 + t255 * t72) + t8 * t171) * t175 
     #* t121 / 0.1440D4 + (t8 * t108 * (t50 - t277 * t72) + 0.90D2 * t76
     # * t31 * (-t277 * t32 + t283 * t50 / 0.2D1 - t283 * t277 * t72 / 0
     #.6D1) + t143 - 0.180D3 * t107 * t108 * (t32 - t277 * t50 + t283 * 
     #t72 / 0.2D1)) * t175 / 0.1440D4
      t304 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t303)
      t306 = 0.1D1 - x1
      t307 = KAPPA2(t306, x2, 0.0D0, 0.10D1, z)
      t308 = s * t307
      t309 = -t306
      t310 = t1 * t309
      t312 = t1 * x1
      t314 = t307 ** 2
      t320 = 0.1D1 / (-0.2D1 + t307)
      t321 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, t306, x2, 0.0D0, 0
     #.10D1)
      t322 = t320 * t321
      t323 = t309 ** 2
      t324 = t16 * t323
      t325 = t314 ** 2
      t327 = t324 * x4 * t325
      t330 = log(0.4D1 * t81 * t327)
      t331 = t330 * t320
      t332 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, t306, x2, 0.0D0, 0
     #.10D1)
      t334 = t330 ** 2
      t336 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, t306, x2, 0.0D0, 0
     #.10D1)
      t343 = t320 * t332
      t349 = t8 * t27
      t351 = t31 * t320 * t336
      t352 = t349 * t351
      t359 = log(0.4D1 * t81 * t324 * t325)
      t360 = t359 * t320
      t366 = t359 ** 2
      t367 = t366 * t320
      t389 = t154 * t14
      t392 = log(0.4D1 * t389 * t327)
      t399 = t107 * t27
      t409 = log(0.4D1 * t155 * t94 * t323 * t325)
      t410 = t409 * t320
      t412 = t409 ** 2
      t429 = (0.90D2 * t76 * t31 * (t322 - t331 * t332 + t334 * t320 * t
     #336 / 0.2D1) - 0.180D3 * t107 * t108 * (t343 - t331 * t336) + t352
     #) * t119 * t121 / 0.720D3 - (-t8 * t108 * (t343 - t360 * t336) - 0
     #.90D2 * t76 * t31 * (-t360 * t321 + t367 * t332 / 0.2D1 - t366 * t
     #359 * t320 * t336 / 0.6D1) - t41 * t27 * t351 + 0.180D3 * t107 * t
     #108 * (t322 - t360 * t332 + t367 * t336 / 0.2D1)) * t119 / 0.720D3
     # - (0.90D2 * t76 * t31 * (-t343 + t392 * t320 * t336) + 0.180D3 * 
     #t399 * t351) * t175 * t177 / 0.720D3 - (0.90D2 * t76 * t31 * (-t32
     #2 + t410 * t332 - t412 * t320 * t336 / 0.2D1) - 0.180D3 * t107 * t
     #108 * (-t343 + t410 * t336) - t352) * t175 * t119 / 0.720D3
      t430 = FJET(XB1, XB2, s, 0.0D0, -t308 * t310, t308 * t312, 0.0D0, 
     #-s * t314 * t15 * t309 * x1, t429)
      t432 = KAPPA2(t306, x2, 0.0D0, t77, z)
      t433 = s * t432
      t435 = t312 * t83
      t437 = t312 * x4
      t439 = t432 ** 2
      t442 = t309 * x1
      t446 = 0.1D1 / (-0.2D1 + t432)
      t447 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, t306, x2, 0.0D0, t
     #77)
      t450 = t439 ** 2
      t455 = log(-0.4D1 * t124 * t323 * x4 * t83 * t450)
      t456 = t455 * t446
      t457 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, t306, x2, 0.0D0, t
     #77)
      t459 = t455 ** 2
      t461 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, t306, x2, 0.0D0, t
     #77)
      t468 = t446 * t457
      t475 = t31 * t446 * t461
      t484 = log(-0.4D1 * t389 * t324 * t160 * t450)
      t497 = (-0.90D2 * t76 * t31 * (t446 * t447 - t456 * t457 + t459 * 
     #t446 * t461 / 0.2D1) + 0.180D3 * t107 * t108 * (t468 - t456 * t461
     #) - t349 * t475) * t119 * t121 / 0.720D3 - (0.90D2 * t76 * t31 * (
     #t468 - t484 * t446 * t461) - 0.180D3 * t399 * t475) * t175 * t177 
     #/ 0.720D3
      t498 = FJET(XB1, XB2, s, 0.0D0, -t433 * t310, -t433 * t435, t433 *
     # t437, s * t439 * t15 * t442 * t83, t497)
      t501 = -0.1D1 + x3
      t503 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, t7
     #7)
      t504 = t16 * t501
      t505 = t504 * t160
      t508 = log(0.4D1 * t245 * t505)
      t509 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, t7
     #7)
      t511 = t508 ** 2
      t512 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, t7
     #7)
      t515 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #10D1)
      t519 = log(-0.4D1 * t245 * t504 * x4)
      t520 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #10D1)
      t522 = t519 ** 2
      t523 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #10D1)
      t537 = t108 * (-t512 + t523)
      t543 = t94 * t501
      t546 = log(-0.4D1 * t244 * t543)
      t552 = t546 ** 2
      t562 = t108 * t523
      t576 = log(0.4D1 * t389 * t505)
      t578 = t501 * x4
      t582 = log(-0.4D1 * t155 * t94 * t578)
      t596 = log(-0.4D1 * t155 * t543)
      t598 = t596 ** 2
      t615 = -(0.90D2 * t76 * t31 * (-t503 + t508 * t509 - t511 * t512 /
     # 0.2D1 + t515 - t519 * t520 + t522 * t523 / 0.2D1) - 0.180D3 * t10
     #7 * t108 * (-t509 + t508 * t512 + t520 - t519 * t523) + t8 * t537)
     # * t175 * t121 / 0.1440D4 + (-t8 * t108 * (t520 - t546 * t523) - 0
     #.90D2 * t76 * t31 * (-t546 * t515 + t552 * t520 / 0.2D1 - t552 * t
     #546 * t523 / 0.6D1) - t41 * t562 + 0.180D3 * t107 * t108 * (t515 -
     # t546 * t520 + t552 * t523 / 0.2D1)) * t175 / 0.1440D4 - (0.90D2 *
     # t76 * t31 * (-t509 + t576 * t512 + t520 - t582 * t523) - 0.180D3 
     #* t107 * t537) * t175 * t177 / 0.720D3 - (0.90D2 * t76 * t31 * (t5
     #15 - t596 * t520 + t598 * t523 / 0.2D1) - 0.180D3 * t107 * t108 * 
     #(t520 - t596 * t523) + t8 * t562) * t175 * t119 / 0.720D3
      t616 = FJET(XB1, XB2, s, t2 * x3, -t2 * t501, 0.0D0, 0.0D0, 0.0D0,
     # t615)
      t618 = KAPPA2(t306, x2, x3, 0.10D1, z)
      t619 = s * t618
      t620 = t310 * x3
      t622 = t310 * t501
      t625 = t618 ** 2
      t631 = 0.1D1 / (-0.2D1 + t618)
      t632 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, t306, x2, x3, 0.10
     #D1)
      t633 = t631 * t632
      t634 = t625 ** 2
      t639 = log(-0.4D1 * t389 * t324 * t578 * t634)
      t641 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, t306, x2, x3, 0.10
     #D1)
      t648 = t31 * t631 * t641
      t654 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, t306, x2, x3, 0.10
     #D1)
      t660 = log(-0.4D1 * t389 * t324 * t501 * t634)
      t661 = t660 * t631
      t663 = t660 ** 2
      t681 = -(0.90D2 * t76 * t31 * (t633 - t639 * t631 * t641) - 0.180D
     #3 * t399 * t648) * t175 * t177 / 0.720D3 - (0.90D2 * t76 * t31 * (
     #t631 * t654 - t661 * t632 + t663 * t631 * t641 / 0.2D1) - 0.180D3 
     #* t107 * t108 * (t633 - t661 * t641) + t349 * t648) * t175 * t119 
     #/ 0.720D3
      t682 = FJET(XB1, XB2, s, -t619 * t620, t619 * t622, t619 * t312, 0
     #.0D0, s * t625 * t15 * t442 * t501, t681)
      t684 = KAPPA2(t306, x2, x3, t77, z)
      t685 = s * t684
      t690 = t684 ** 2
      t695 = cos(t9)
      t698 = Sqrt(x3 * t501 * t160)
      t705 = 0.1D1 / (-0.2D1 + t684)
      t706 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, t306, x2, x3, t77)
      t709 = t690 ** 2
      t714 = log(0.4D1 * t180 * t323 * t501 * t160 * t709)
      t716 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, t306, x2, x3, t77)
      t726 = -0.90D2 * t76 * t31 * (t705 * t706 - t714 * t705 * t716) + 
     #0.180D3 * t399 * t31 * t705 * t716
      t730 = FJET(XB1, XB2, s, -t685 * t620, t685 * t622, -t685 * t435, 
     #t685 * t437, s * t690 * t15 * t442 * (-0.1D1 + x3 + x4 - 0.2D1 * x
     #3 * x4 + 0.2D1 * t695 * t698), -t726 * t175 * t177 / 0.720D3)
      rrqq2qqht3s3e1 = t304 * t303 + t430 * t429 + t498 * t497 + t616 * 
     #t615 + t682 * t681 - t730 * t726 * t175 * t119 * t121 / 0.720D3

      end function



      doubleprecision function rrqq2qqht3s3e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.3141592653589793D1 * lh
      t5 = x2 * 0.3141592653589793D1
      t6 = sin(t5)
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
      t24 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t28 = 0.3141592653589793D1 ** 2
      t30 = lh ** 2
      t32 = -0.30D2 * t28 + 0.180D3 * t30
      t33 = 0.3141592653589793D1 * t32
      t36 = t15 ** 2
      t37 = t36 * 0.3141592653589793D1
      t41 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t59 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t63 = t19 * 0.3141592653589793D1
      t64 = 0.1D1 - x4
      t65 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #t64)
      t66 = x3 * t7
      t67 = t66 * t9
      t68 = t12 * x4
      t69 = -t64
      t70 = t68 * t69
      t73 = log(-0.4D1 * t67 * t70)
      t74 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #t64)
      t76 = t9 * t12
      t77 = t76 * x4
      t80 = log(0.4D1 * t66 * t77)
      t86 = t19 * t23
      t87 = t74 - t59
      t92 = 0.1D1 / x3
      t94 = 0.1D1 / x4
      t99 = log(0.4D1 * t66 * t76)
      t101 = t99 ** 2
      t113 = t86 * t59
      t114 = t33 * t113
      t118 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # t64)
      t121 = log(-0.4D1 * t10 * t70)
      t123 = t121 ** 2
      t128 = log(0.4D1 * t10 * t68)
      t130 = t128 ** 2
      t144 = -t86 * t87
      t149 = x1 ** 2
      t150 = t149 * t7
      t151 = t150 * t9
      t154 = log(-0.4D1 * t151 * t70)
      t158 = log(0.4D1 * t150 * t77)
      t167 = 0.1D1 / x1
      t171 = t63 * t23
      t173 = t167 * t94
      t177 = x3 * t149
      t180 = log(0.4D1 * t177 * t13)
      t192 = t150 * t76
      t194 = log(0.4D1 * t192)
      t196 = t194 ** 2
      t211 = (-0.180D3 * t3 - 0.90D2 * t16) * t19 * t23 * t24 / 0.1440D4
     # + (t33 + 0.180D3 * t16 * lh + 0.45D2 * t37) * t19 * t23 * t41 / 0
     #.1440D4 + (0.3141592653589793D1 * (-0.2884936567583026D3 - 0.120D3
     # * t30 * lh + 0.60D2 * lh * t28) - t16 * t32 - 0.90D2 * t37 * lh -
     # 0.15D2 * t36 * t15 * 0.3141592653589793D1) * t19 * t23 * t59 / 0.
     #1440D4 - (0.90D2 * t63 * t23 * (t65 - t73 * t74 - t41 + t80 * t59)
     # - 0.180D3 * t3 * t86 * t87) * t92 * t94 / 0.1440D4 + (0.90D2 * t6
     #3 * t23 * (t24 - t99 * t41 + t101 * t59 / 0.2D1) - 0.180D3 * t3 * 
     #t86 * (t41 - t99 * t59) + t114) * t92 / 0.1440D4 + (0.90D2 * t63 *
     # t23 * (-t118 + t121 * t65 - t123 * t74 / 0.2D1 + t24 - t128 * t41
     # + t130 * t59 / 0.2D1) - 0.180D3 * t3 * t86 * (-t65 + t121 * t74 +
     # t41 - t128 * t59) + t33 * t144) * t94 / 0.1440D4 + (0.90D2 * t63 
     #* t23 * (-t65 + t154 * t74 + t41 - t158 * t59) - 0.180D3 * t3 * t1
     #44) * t167 * t94 / 0.720D3 - t171 * t87 * t92 * t173 / 0.8D1 - (0.
     #90D2 * t63 * t23 * (-t41 + t180 * t59) + 0.180D3 * t3 * t113) * t9
     #2 * t167 / 0.720D3 - (0.90D2 * t63 * t23 * (-t24 + t194 * t41 - t1
     #96 * t59 / 0.2D1) - 0.180D3 * t3 * t86 * (-t41 + t194 * t59) - t11
     #4) * t167 / 0.720D3
      t212 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t211)
      t214 = 0.1D1 - x1
      t215 = KAPPA2(t214, x2, 0.0D0, 0.10D1, z)
      t216 = s * t215
      t217 = -t214
      t218 = t1 * t217
      t220 = t1 * x1
      t222 = t215 ** 2
      t228 = 0.1D1 / (-0.2D1 + t215)
      t229 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, t214, x2, 0.0D0, 0
     #.10D1)
      t230 = t228 * t229
      t231 = t217 ** 2
      t232 = t12 * t231
      t233 = t222 ** 2
      t238 = log(0.4D1 * t151 * t232 * x4 * t233)
      t240 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, t214, x2, 0.0D0, 0
     #.10D1)
      t246 = t3 * t19
      t247 = t23 * t228
      t248 = t247 * t240
      t250 = 0.180D3 * t246 * t248
      t260 = t177 * t7
      t265 = log(0.4D1 * t260 * t76 * t231 * t233)
      t276 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, t214, x2, 0.0D0, 0
     #.10D1)
      t281 = log(0.4D1 * t151 * t232 * t233)
      t282 = t281 * t228
      t284 = t281 ** 2
      t302 = (0.90D2 * t63 * t23 * (t230 - t238 * t228 * t240) - t250) *
     # t167 * t94 / 0.720D3 + t63 * t247 * t240 * t92 * t173 / 0.8D1 - (
     #0.90D2 * t63 * t23 * (-t230 + t265 * t228 * t240) + t250) * t92 * 
     #t167 / 0.720D3 - (-0.90D2 * t63 * t23 * (t228 * t276 - t282 * t229
     # + t284 * t228 * t240 / 0.2D1) + 0.180D3 * t3 * t86 * (t230 - t282
     # * t240) - t33 * t19 * t248) * t167 / 0.720D3
      t303 = FJET(XB1, XB2, s, 0.0D0, -t216 * t218, t216 * t220, 0.0D0, 
     #-s * t222 * t11 * t217 * x1, t302)
      t305 = KAPPA2(t214, x2, 0.0D0, t64, z)
      t306 = s * t305
      t308 = t220 * t69
      t310 = t220 * x4
      t312 = t305 ** 2
      t315 = t217 * x1
      t319 = 0.1D1 / (-0.2D1 + t305)
      t320 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, t214, x2, 0.0D0, t
     #64)
      t323 = t312 ** 2
      t328 = log(-0.4D1 * t192 * t231 * x4 * t69 * t323)
      t330 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, t214, x2, 0.0D0, t
     #64)
      t336 = t23 * t319
      t349 = (-0.90D2 * t63 * t23 * (t319 * t320 - t328 * t319 * t330) +
     # 0.180D3 * t246 * t336 * t330) * t167 * t94 / 0.720D3 - t63 * t336
     # * t330 * t92 * t173 / 0.8D1
      t350 = FJET(XB1, XB2, s, 0.0D0, -t306 * t218, -t306 * t308, t306 *
     # t310, s * t312 * t11 * t315 * t69, t349)
      t353 = -0.1D1 + x3
      t355 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, t6
     #4)
      t356 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #10D1)
      t357 = -t355 + t356
      t362 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #10D1)
      t363 = t76 * t353
      t366 = log(-0.4D1 * t260 * t363)
      t372 = t86 * t356
      t379 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, t6
     #4)
      t380 = t12 * t353
      t381 = x4 * t69
      t385 = log(0.4D1 * t67 * t380 * t381)
      t390 = log(-0.4D1 * t67 * t380 * x4)
      t403 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #10D1)
      t406 = log(-0.4D1 * t66 * t363)
      t408 = t406 ** 2
      t424 = -t171 * t357 * t92 * t173 / 0.8D1 - (0.90D2 * t63 * t23 * (
     #t362 - t366 * t356) - 0.180D3 * t3 * t372) * t92 * t167 / 0.720D3 
     #- (0.90D2 * t63 * t23 * (-t379 + t385 * t355 + t362 - t390 * t356)
     # - 0.180D3 * t3 * t86 * t357) * t92 * t94 / 0.1440D4 + (-0.90D2 * 
     #t63 * t23 * (t403 - t406 * t362 + t408 * t356 / 0.2D1) + 0.180D3 *
     # t3 * t86 * (t362 - t406 * t356) - t33 * t372) * t92 / 0.1440D4
      t425 = FJET(XB1, XB2, s, t2 * x3, -t2 * t353, 0.0D0, 0.0D0, 0.0D0,
     # t424)
      t427 = KAPPA2(t214, x2, x3, 0.10D1, z)
      t428 = s * t427
      t429 = t218 * x3
      t431 = t218 * t353
      t434 = t427 ** 2
      t440 = 0.1D1 / (-0.2D1 + t427)
      t441 = t23 * t440
      t443 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, t214, x2, x3, 0.10
     #D1)
      t448 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, t214, x2, x3, 0.10
     #D1)
      t451 = t434 ** 2
      t456 = log(-0.4D1 * t177 * t10 * t232 * t353 * t451)
      t470 = -t63 * t441 * t443 * t92 * t173 / 0.8D1 - (0.90D2 * t63 * t
     #23 * (t440 * t448 - t456 * t440 * t443) - 0.180D3 * t246 * t441 * 
     #t443) * t92 * t167 / 0.720D3
      t471 = FJET(XB1, XB2, s, -t428 * t429, t428 * t431, t428 * t220, 0
     #.0D0, s * t434 * t11 * t315 * t353, t470)
      t473 = KAPPA2(t214, x2, x3, t64, z)
      t474 = s * t473
      t479 = t473 ** 2
      t484 = cos(t5)
      t487 = Sqrt(x3 * t353 * t381)
      t494 = 0.1D1 / (-0.2D1 + t473)
      t497 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, t214, x2, x3, t64)
      t502 = FJET(XB1, XB2, s, -t474 * t429, t474 * t431, -t474 * t308, 
     #t474 * t310, s * t479 * t11 * t315 * (-0.1D1 + x3 + x4 - 0.2D1 * x
     #3 * x4 + 0.2D1 * t484 * t487), t63 * t23 * t494 * t497 * t92 * t17
     #3 / 0.8D1)
      rrqq2qqht3s3e0 = t212 * t211 + t303 * t302 + t350 * t349 + t425 * 
     #t424 + t471 * t470 + t502 * 0.3141592653589793D1 * t86 * t494 * t4
     #97 * t92 * t167 * t94 / 0.8D1

      end function



      doubleprecision function rrqq2qqht3s3em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 0
     #.10D1)
      t12 = 0.3141592653589793D1 * lh
      t15 = sin(x2 * 0.3141592653589793D1)
      t16 = t15 ** 2
      t17 = z ** 2
      t18 = 0.1D1 / t17
      t19 = t16 * t18
      t20 = t1 ** 2
      t21 = t20 ** 2
      t24 = log(0.4D1 * t19 * t21)
      t25 = t24 * 0.3141592653589793D1
      t29 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t33 = 0.3141592653589793D1 ** 2
      t35 = lh ** 2
      t41 = t24 ** 2
      t46 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t50 = t4 * t7
      t51 = 0.1D1 - x4
      t52 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #t51)
      t53 = t52 - t46
      t54 = 0.1D1 / x3
      t56 = 0.1D1 / x4
      t60 = x3 * t16
      t61 = t18 * t21
      t64 = log(0.4D1 * t60 * t61)
      t70 = t3 * t7
      t73 = 0.180D3 * t12 * t70 * t46
      t77 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #t51)
      t78 = t21 * x4
      t79 = -t51
      t83 = log(-0.4D1 * t19 * t78 * t79)
      t87 = log(0.4D1 * t19 * t78)
      t93 = -t53
      t101 = 0.1D1 / x1
      t105 = x1 ** 2
      t106 = t105 * t16
      t109 = log(0.4D1 * t106 * t61)
      t122 = t4 * t7 * t8 / 0.16D2 + (-0.180D3 * t12 - 0.90D2 * t25) * t
     #3 * t7 * t29 / 0.1440D4 + (0.3141592653589793D1 * (-0.30D2 * t33 +
     # 0.180D3 * t35) + 0.180D3 * t25 * lh + 0.45D2 * t41 * 0.3141592653
     #589793D1) * t3 * t7 * t46 / 0.1440D4 - t50 * t53 * t54 * t56 / 0.1
     #6D2 + (0.90D2 * t4 * t7 * (t29 - t64 * t46) - t73) * t54 / 0.1440D
     #4 + (0.90D2 * t4 * t7 * (-t77 + t83 * t52 + t29 - t87 * t46) - 0.1
     #80D3 * t12 * t70 * t93) * t56 / 0.1440D4 + t50 * t46 * t54 * t101 
     #/ 0.8D1 - (0.90D2 * t4 * t7 * (-t29 + t109 * t46) + t73) * t101 / 
     #0.720D3 + t50 * t93 * t101 * t56 / 0.8D1
      t123 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t122)
      t125 = 0.1D1 - x1
      t126 = KAPPA2(t125, x2, 0.0D0, 0.10D1, z)
      t127 = s * t126
      t128 = -t125
      t129 = t1 * t128
      t131 = t1 * x1
      t133 = t126 ** 2
      t139 = 0.1D1 / (-0.2D1 + t126)
      t140 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, t125, x2, 0.0D0, 0
     #.10D1)
      t141 = t139 * t140
      t142 = t54 * t101
      t146 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, t125, x2, 0.0D0, 0
     #.10D1)
      t149 = t128 ** 2
      t151 = t133 ** 2
      t155 = log(0.4D1 * t106 * t18 * t21 * t149 * t151)
      t170 = t101 * t56
      t174 = t50 * t141 * t142 / 0.8D1 - (-0.90D2 * t4 * t7 * (t139 * t1
     #46 - t155 * t139 * t140) + 0.180D3 * t12 * t3 * t7 * t139 * t140) 
     #* t101 / 0.720D3 + t50 * t141 * t170 / 0.8D1
      t175 = FJET(XB1, XB2, s, 0.0D0, -t127 * t129, t127 * t131, 0.0D0, 
     #-s * t133 * t20 * t128 * x1, t174)
      t177 = KAPPA2(t125, x2, 0.0D0, t51, z)
      t178 = s * t177
      t184 = t177 ** 2
      t187 = t128 * x1
      t192 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, t125, x2, 0.0D0, t
     #51)
      t194 = 0.1D1 / (-0.2D1 + t177) * t192 * t170
      t197 = FJET(XB1, XB2, s, 0.0D0, -t178 * t129, -t178 * t131 * t79, 
     #t178 * t131 * x4, s * t184 * t20 * t187 * t79, -t50 * t194 / 0.8D1
     #)
      t203 = -0.1D1 + x3
      t205 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #10D1)
      t210 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, t5
     #1)
      t216 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #10D1)
      t220 = log(-0.4D1 * t60 * t61 * t203)
      t232 = -t50 * t205 * t54 * t101 / 0.8D1 - t50 * (-t210 + t205) * t
     #54 * t56 / 0.16D2 + (-0.90D2 * t4 * t7 * (t216 - t220 * t205) + 0.
     #180D3 * t12 * t70 * t205) * t54 / 0.1440D4
      t233 = FJET(XB1, XB2, s, t2 * x3, -t2 * t203, 0.0D0, 0.0D0, 0.0D0,
     # t232)
      t235 = KAPPA2(t125, x2, x3, 0.10D1, z)
      t236 = s * t235
      t242 = t235 ** 2
      t249 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, t125, x2, x3, 0.10
     #D1)
      t251 = 0.1D1 / (-0.2D1 + t235) * t249 * t142
      t254 = FJET(XB1, XB2, s, -t236 * t129 * x3, t236 * t129 * t203, t2
     #36 * t131, 0.0D0, s * t242 * t20 * t187 * t203, -t50 * t251 / 0.8D
     #1)
      rrqq2qqht3s3em1 = t123 * t122 + t175 * t174 - t197 * 0.31415926535
     #89793D1 * t70 * t194 / 0.8D1 + t233 * t232 - t254 * 0.314159265358
     #9793D1 * t70 * t251 / 0.8D1

      end function



      doubleprecision function rrqq2qqht3s3em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t9 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 0
     #.1D1 - x4)
      t10 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
      t17 = t7 * t10
      t18 = 0.1D1 / x3
      t22 = 0.1D1 / x1
      t26 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.10D1)
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
      t50 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t49)
      t55 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.1
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
      t81 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, t65, x2, 0.0D0, 0.1
     #0D1)
      t86 = FJET(XB1, XB2, s, 0.0D0, -t67 * t1 * t68, t67 * t1 * x1, 0.0
     #D0, -s * t73 * t38 * t68 * x1, t4 * t7 * t80 * t81 * t22 / 0.8D1)
      rrqq2qqht3s3em2 = t50 * t49 - t60 * 0.3141592653589793D1 * t3 * t5
     #7 / 0.16D2 + t86 * 0.3141592653589793D1 * t3 * t7 * t80 * t81 * t2
     #2 / 0.8D1

      end function



      doubleprecision function rrqq2qqht3s3em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      t1 = z - 0.1D1
      t3 = 0.1D1 / t1
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 0
     #.10D1)
      t12 = FJET(XB1, XB2, s, 0.0D0, s * t1, 0.0D0, 0.0D0, 0.0D0, 0.3141
     #592653589793D1 * t3 * t7 * t8 / 0.16D2)
      rrqq2qqht3s3em3 = t12 * 0.3141592653589793D1 * t3 * t7 * t8 / 0.16
     #D2

      end function



      doubleprecision function rrqq2qqht3s3em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      rrqq2qqht3s3em4 = 0.0D0

      end function


      doubleprecision function rrqq2qqht3s4e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.3141592653589793D1 ** 2
      t5 = lh ** 2
      t7 = -0.30D2 * t3 + 0.180D3 * t5
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
      t32 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.0D0)
      t40 = -0.2884936567583026D3 - 0.120D3 * t5 * lh + 0.60D2 * lh * t3
      t41 = 0.3141592653589793D1 * t40
      t46 = t23 * t19 * 0.3141592653589793D1
      t50 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.0D0)
      t54 = t3 ** 2
      t55 = t5 ** 2
      t67 = t23 ** 2
      t72 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.0D0)
      t76 = 0.3141592653589793D1 * t27
      t77 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #x4)
      t78 = x1 ** 2
      t79 = t78 * t11
      t80 = t79 * t13
      t81 = t16 * x4
      t82 = -0.1D1 + x4
      t83 = t81 * t82
      t86 = log(-0.4D1 * t80 * t83)
      t87 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #x4)
      t89 = t86 ** 2
      t90 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #x4)
      t93 = t13 * t16
      t94 = t93 * x4
      t97 = log(0.4D1 * t79 * t94)
      t99 = t97 ** 2
      t106 = 0.3141592653589793D1 * lh
      t107 = t27 * t31
      t114 = -t72 + t90
      t115 = t107 * t114
      t116 = t8 * t115
      t118 = 0.1D1 / x1
      t120 = 0.1D1 / x4
      t123 = t79 * t93
      t125 = log(0.4D1 * t123)
      t131 = t125 ** 2
      t141 = t107 * t72
      t142 = t41 * t141
      t153 = x3 * t78
      t154 = t153 * t11
      t155 = x4 * t82
      t159 = log(-0.4D1 * t154 * t93 * t155)
      t163 = log(0.4D1 * t154 * t94)
      t174 = 0.1D1 / x3
      t176 = t118 * t120
      t179 = t153 * t17
      t181 = log(0.4D1 * t179)
      t183 = t181 ** 2
      t202 = log(0.4D1 * t14 * t81)
      t206 = log(-0.4D1 * t14 * t83)
      t212 = t206 ** 2
      t219 = t202 ** 2
      t243 = x3 * t11
      t246 = log(0.4D1 * t243 * t94)
      t248 = t246 ** 2
      t251 = t243 * t13
      t254 = log(-0.4D1 * t251 * t83)
      t256 = t254 ** 2
      t275 = log(0.4D1 * t243 * t93)
      t281 = t275 ** 2
      t301 = (t8 + 0.180D3 * t20 * lh + 0.45D2 * t24) * t27 * t31 * t32 
     #/ 0.1440D4 + (t41 - t20 * t7 - 0.90D2 * t24 * lh - 0.15D2 * t46) *
     # t27 * t31 * t50 / 0.1440D4 + (0.3141592653589793D1 * (t54 + 0.60D
     #2 * t55 + 0.5769873135166051D3 * lh - 0.60D2 * t5 * t3) - t20 * t4
     #0 + t24 * t7 / 0.2D1 + 0.30D2 * t46 * lh + 0.15D2 / 0.4D1 * t67 * 
     #0.3141592653589793D1) * t27 * t31 * t72 / 0.1440D4 - (0.90D2 * t76
     # * t31 * (t77 - t86 * t87 + t89 * t90 / 0.2D1 - t32 + t97 * t50 - 
     #t99 * t72 / 0.2D1) - 0.180D3 * t106 * t107 * (t87 - t86 * t90 - t5
     #0 + t97 * t72) + t116) * t118 * t120 / 0.720D3 + (t8 * t107 * (t50
     # - t125 * t72) + 0.90D2 * t76 * t31 * (-t125 * t32 + t131 * t50 / 
     #0.2D1 - t131 * t125 * t72 / 0.6D1) + t142 - 0.180D3 * t106 * t107 
     #* (t32 - t125 * t50 + t131 * t72 / 0.2D1)) * t118 / 0.720D3 + (0.9
     #0D2 * t76 * t31 * (-t87 + t159 * t90 + t50 - t163 * t72) + 0.180D3
     # * t106 * t107 * t114) * t174 * t176 / 0.720D3 - (0.90D2 * t76 * t
     #31 * (-t32 + t181 * t50 - t183 * t72 / 0.2D1) - 0.180D3 * t106 * t
     #107 * (-t50 + t181 * t72) - t8 * t141) * t174 * t118 / 0.720D3 - (
     #t8 * t107 * (-t50 + t202 * t72 + t87 - t206 * t90) + 0.90D2 * t76 
     #* t31 * (-t206 * t77 + t212 * t87 / 0.2D1 - t212 * t206 * t90 / 0.
     #6D1 + t202 * t32 - t219 * t50 / 0.2D1 + t219 * t202 * t72 / 0.6D1)
     # + t41 * t115 - 0.180D3 * t106 * t107 * (-t32 + t202 * t50 - t219 
     #* t72 / 0.2D1 + t77 - t206 * t87 + t212 * t90 / 0.2D1)) * t120 / 0
     #.1440D4 - (0.90D2 * t76 * t31 * (-t32 + t246 * t50 - t248 * t72 / 
     #0.2D1 + t77 - t254 * t87 + t256 * t90 / 0.2D1) - 0.180D3 * t106 * 
     #t107 * (-t50 + t246 * t72 + t87 - t254 * t90) + t116) * t174 * t12
     #0 / 0.1440D4 + (t8 * t107 * (t50 - t275 * t72) + 0.90D2 * t76 * t3
     #1 * (-t275 * t32 + t281 * t50 / 0.2D1 - t281 * t275 * t72 / 0.6D1)
     # + t142 - 0.180D3 * t106 * t107 * (t32 - t275 * t50 + t281 * t72 /
     # 0.2D1)) * t174 / 0.1440D4
      t302 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t301)
      t304 = -0.1D1 + x1
      t307 = -t304
      t308 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, t307, x2, 0.0D0, 0
     #.0D0)
      t309 = t304 ** 2
      t310 = t16 * t309
      t314 = log(0.4D1 * t80 * t310 * x4)
      t315 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, t307, x2, 0.0D0, 0
     #.0D0)
      t317 = t314 ** 2
      t318 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, t307, x2, 0.0D0, 0
     #.0D0)
      t330 = t107 * t318
      t331 = t8 * t330
      t335 = t93 * t309
      t338 = log(0.4D1 * t79 * t335)
      t344 = t338 ** 2
      t364 = t309 * x4
      t368 = log(0.4D1 * t154 * t93 * t364)
      t381 = log(0.4D1 * t154 * t335)
      t383 = t381 ** 2
      t399 = -(0.90D2 * t76 * t31 * (t308 - t314 * t315 + t317 * t318 / 
     #0.2D1) - 0.180D3 * t106 * t107 * (t315 - t314 * t318) + t331) * t1
     #18 * t120 / 0.720D3 + (-t8 * t107 * (t315 - t338 * t318) - 0.90D2 
     #* t76 * t31 * (-t338 * t308 + t344 * t315 / 0.2D1 - t344 * t338 * 
     #t318 / 0.6D1) - t41 * t330 + 0.180D3 * t106 * t107 * (t308 - t338 
     #* t315 + t344 * t318 / 0.2D1)) * t118 / 0.720D3 + (0.90D2 * t76 * 
     #t31 * (-t315 + t368 * t318) + 0.180D3 * t106 * t330) * t174 * t176
     # / 0.720D3 - (0.90D2 * t76 * t31 * (t308 - t381 * t315 + t383 * t3
     #18 / 0.2D1) - 0.180D3 * t106 * t107 * (t315 - t381 * t318) + t331)
     # * t174 * t118 / 0.720D3
      t400 = FJET(XB1, XB2, s, 0.0D0, -t2 * t304, 0.0D0, t2 * x1, 0.0D0,
     # t399)
      t402 = KAPPA2(t307, x2, 0.0D0, x4, z)
      t403 = s * t402
      t404 = t1 * t304
      t406 = t1 * x1
      t407 = t406 * x4
      t409 = t406 * t82
      t411 = t402 ** 2
      t414 = t304 * x1
      t418 = 0.1D1 / (-0.2D1 + t402)
      t419 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, t307, x2, 0.0D0, x
     #4)
      t421 = t411 ** 2
      t426 = log(-0.4D1 * t123 * t364 * t82 * t421)
      t427 = t426 * t418
      t428 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, t307, x2, 0.0D0, x
     #4)
      t430 = t426 ** 2
      t432 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, t307, x2, 0.0D0, x
     #4)
      t439 = t418 * t428
      t445 = t8 * t27
      t447 = t31 * t418 * t432
      t452 = t153 * t14
      t457 = log(-0.4D1 * t452 * t310 * t155 * t421)
      t464 = t106 * t27
      t471 = -(0.90D2 * t76 * t31 * (t418 * t419 - t427 * t428 + t430 * 
     #t418 * t432 / 0.2D1) - 0.180D3 * t106 * t107 * (t439 - t427 * t432
     #) + t445 * t447) * t118 * t120 / 0.720D3 + (0.90D2 * t76 * t31 * (
     #-t439 + t457 * t418 * t432) + 0.180D3 * t464 * t447) * t174 * t176
     # / 0.720D3
      t472 = FJET(XB1, XB2, s, 0.0D0, -t403 * t404, t403 * t407, -t403 *
     # t409, -s * t411 * t15 * t414 * x4, t471)
      t475 = -0.1D1 + x3
      t477 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t478 = t16 * t475
      t479 = t478 * t155
      t482 = log(0.4D1 * t251 * t479)
      t483 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t485 = t482 ** 2
      t486 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t489 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #0D0)
      t493 = log(-0.4D1 * t251 * t478 * x4)
      t494 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #0D0)
      t496 = t493 ** 2
      t497 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #0D0)
      t510 = t497 - t486
      t517 = t93 * t475
      t520 = log(-0.4D1 * t243 * t517)
      t526 = t520 ** 2
      t536 = t107 * t497
      t548 = t475 * x4
      t552 = log(-0.4D1 * t154 * t93 * t548)
      t556 = log(0.4D1 * t452 * t479)
      t572 = log(-0.4D1 * t154 * t517)
      t574 = t572 ** 2
      t591 = -(0.90D2 * t76 * t31 * (-t477 + t482 * t483 - t485 * t486 /
     # 0.2D1 + t489 - t493 * t494 + t496 * t497 / 0.2D1) - 0.180D3 * t10
     #6 * t107 * (-t483 + t482 * t486 + t494 - t493 * t497) + t8 * t107 
     #* t510) * t174 * t120 / 0.1440D4 + (-t8 * t107 * (t494 - t520 * t4
     #97) - 0.90D2 * t76 * t31 * (-t520 * t489 + t526 * t494 / 0.2D1 - t
     #526 * t520 * t497 / 0.6D1) - t41 * t536 + 0.180D3 * t106 * t107 * 
     #(t489 - t520 * t494 + t526 * t497 / 0.2D1)) * t174 / 0.1440D4 + (0
     #.90D2 * t76 * t31 * (-t494 + t552 * t497 + t483 - t556 * t486) + 0
     #.180D3 * t106 * t107 * t510) * t174 * t176 / 0.720D3 - (0.90D2 * t
     #76 * t31 * (t489 - t572 * t494 + t574 * t497 / 0.2D1) - 0.180D3 * 
     #t106 * t107 * (t494 - t572 * t497) + t8 * t536) * t174 * t118 / 0.
     #720D3
      t592 = FJET(XB1, XB2, s, t2 * x3, -t2 * t475, 0.0D0, 0.0D0, 0.0D0,
     # t591)
      t594 = KAPPA2(t307, x2, x3, 0.0D0, z)
      t595 = s * t594
      t596 = t404 * x3
      t598 = t404 * t475
      t601 = t594 ** 2
      t607 = 0.1D1 / (-0.2D1 + t594)
      t608 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, t307, x2, x3, 0.0D
     #0)
      t609 = t607 * t608
      t610 = t601 ** 2
      t615 = log(-0.4D1 * t452 * t310 * t548 * t610)
      t617 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, t307, x2, x3, 0.0D
     #0)
      t624 = t31 * t607 * t617
      t630 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, t307, x2, x3, 0.0D
     #0)
      t636 = log(-0.4D1 * t452 * t310 * t475 * t610)
      t637 = t636 * t607
      t639 = t636 ** 2
      t657 = (0.90D2 * t76 * t31 * (-t609 + t615 * t607 * t617) + 0.180D
     #3 * t464 * t624) * t174 * t176 / 0.720D3 - (0.90D2 * t76 * t31 * (
     #t607 * t630 - t637 * t608 + t639 * t607 * t617 / 0.2D1) - 0.180D3 
     #* t106 * t107 * (t609 - t637 * t617) + t445 * t624) * t174 * t118 
     #/ 0.720D3
      t658 = FJET(XB1, XB2, s, -t595 * t596, t595 * t598, 0.0D0, t595 * 
     #t406, -s * t601 * t15 * t414 * x3, t657)
      t660 = KAPPA2(t307, x2, x3, x4, z)
      t661 = s * t660
      t666 = t660 ** 2
      t671 = cos(t9)
      t674 = Sqrt(x3 * t475 * t155)
      t681 = 0.1D1 / (-0.2D1 + t660)
      t682 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, t307, x2, x3, x4)
      t685 = t666 ** 2
      t690 = log(0.4D1 * t179 * t309 * t475 * t155 * t685)
      t692 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, t307, x2, x3, x4)
      t702 = 0.90D2 * t76 * t31 * (t682 * t681 - t690 * t681 * t692) - 0
     #.180D3 * t464 * t31 * t681 * t692
      t706 = FJET(XB1, XB2, s, -t661 * t596, t661 * t598, t661 * t407, -
     #t661 * t409, s * t666 * t15 * t414 * (-x3 - x4 + 0.2D1 * x3 * x4 +
     # 0.2D1 * t671 * t674), t702 * t174 * t176 / 0.720D3)
      rrqq2qqht3s4e1 = t302 * t301 + t400 * t399 + t472 * t471 + t592 * 
     #t591 + t658 * t657 + t706 * t702 * t174 * t118 * t120 / 0.720D3

      end function



      doubleprecision function rrqq2qqht3s4e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.3141592653589793D1 * lh
      t5 = x2 * 0.3141592653589793D1
      t6 = sin(t5)
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
      t24 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.0D0)
      t28 = 0.3141592653589793D1 ** 2
      t30 = lh ** 2
      t32 = -0.30D2 * t28 + 0.180D3 * t30
      t33 = 0.3141592653589793D1 * t32
      t36 = t15 ** 2
      t37 = t36 * 0.3141592653589793D1
      t41 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.0D0)
      t59 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.0D0)
      t63 = t19 * 0.3141592653589793D1
      t64 = x3 * t7
      t65 = t9 * t12
      t66 = t65 * x4
      t69 = log(0.4D1 * t64 * t66)
      t71 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #x4)
      t72 = t64 * t9
      t73 = t12 * x4
      t74 = -0.1D1 + x4
      t75 = t73 * t74
      t78 = log(-0.4D1 * t72 * t75)
      t79 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #x4)
      t85 = t19 * t23
      t86 = -t59 + t79
      t87 = t85 * t86
      t89 = 0.180D3 * t3 * t87
      t91 = 0.1D1 / x3
      t93 = 0.1D1 / x4
      t98 = log(0.4D1 * t64 * t65)
      t100 = t98 ** 2
      t112 = t85 * t59
      t113 = t33 * t112
      t119 = log(0.4D1 * t10 * t73)
      t121 = t119 ** 2
      t124 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t127 = log(-0.4D1 * t10 * t75)
      t129 = t127 ** 2
      t146 = x1 ** 2
      t147 = t146 * t7
      t148 = t147 * t9
      t151 = log(-0.4D1 * t148 * t75)
      t155 = log(0.4D1 * t147 * t66)
      t162 = 0.1D1 / x1
      t166 = t63 * t23
      t169 = t162 * t93
      t173 = x3 * t146
      t176 = log(0.4D1 * t173 * t13)
      t188 = t147 * t65
      t190 = log(0.4D1 * t188)
      t192 = t190 ** 2
      t207 = (-0.180D3 * t3 - 0.90D2 * t16) * t19 * t23 * t24 / 0.1440D4
     # + (t33 + 0.180D3 * t16 * lh + 0.45D2 * t37) * t19 * t23 * t41 / 0
     #.1440D4 + (0.3141592653589793D1 * (-0.2884936567583026D3 - 0.120D3
     # * t30 * lh + 0.60D2 * lh * t28) - t16 * t32 - 0.90D2 * t37 * lh -
     # 0.15D2 * t36 * t15 * 0.3141592653589793D1) * t19 * t23 * t59 / 0.
     #1440D4 - (0.90D2 * t63 * t23 * (-t41 + t69 * t59 + t71 - t78 * t79
     #) - t89) * t91 * t93 / 0.1440D4 + (0.90D2 * t63 * t23 * (t24 - t98
     # * t41 + t100 * t59 / 0.2D1) - 0.180D3 * t3 * t85 * (t41 - t98 * t
     #59) + t113) * t91 / 0.1440D4 - (0.90D2 * t63 * t23 * (-t24 + t119 
     #* t41 - t121 * t59 / 0.2D1 + t124 - t127 * t71 + t129 * t79 / 0.2D
     #1) - 0.180D3 * t3 * t85 * (-t41 + t119 * t59 + t71 - t127 * t79) +
     # t33 * t87) * t93 / 0.1440D4 - (0.90D2 * t63 * t23 * (t71 - t151 *
     # t79 - t41 + t155 * t59) - t89) * t162 * t93 / 0.720D3 - t166 * t8
     #6 * t91 * t169 / 0.8D1 - (0.90D2 * t63 * t23 * (-t41 + t176 * t59)
     # + 0.180D3 * t3 * t112) * t91 * t162 / 0.720D3 + (0.90D2 * t63 * t
     #23 * (t24 - t190 * t41 + t192 * t59 / 0.2D1) - 0.180D3 * t3 * t85 
     #* (t41 - t190 * t59) + t113) * t162 / 0.720D3
      t208 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t207)
      t210 = -0.1D1 + x1
      t213 = -t210
      t214 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, t213, x2, 0.0D0, 0
     #.0D0)
      t215 = t210 ** 2
      t216 = t12 * t215
      t220 = log(0.4D1 * t148 * t216 * x4)
      t221 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, t213, x2, 0.0D0, 0
     #.0D0)
      t227 = t85 * t221
      t229 = 0.180D3 * t3 * t227
      t238 = t173 * t7
      t239 = t65 * t215
      t242 = log(0.4D1 * t238 * t239)
      t252 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, t213, x2, 0.0D0, 0
     #.0D0)
      t255 = log(0.4D1 * t147 * t239)
      t257 = t255 ** 2
      t273 = -(0.90D2 * t63 * t23 * (t214 - t220 * t221) - t229) * t162 
     #* t93 / 0.720D3 - t166 * t221 * t91 * t169 / 0.8D1 - (0.90D2 * t63
     # * t23 * (t214 - t242 * t221) - t229) * t91 * t162 / 0.720D3 + (-0
     #.90D2 * t63 * t23 * (t252 - t255 * t214 + t257 * t221 / 0.2D1) + 0
     #.180D3 * t3 * t85 * (t214 - t255 * t221) - t33 * t227) * t162 / 0.
     #720D3
      t274 = FJET(XB1, XB2, s, 0.0D0, -t2 * t210, 0.0D0, t2 * x1, 0.0D0,
     # t273)
      t276 = KAPPA2(t213, x2, 0.0D0, x4, z)
      t277 = s * t276
      t278 = t1 * t210
      t280 = t1 * x1
      t281 = t280 * x4
      t283 = t280 * t74
      t285 = t276 ** 2
      t288 = t210 * x1
      t292 = 0.1D1 / (-0.2D1 + t276)
      t293 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, t213, x2, 0.0D0, x
     #4)
      t296 = t285 ** 2
      t301 = log(-0.4D1 * t188 * t215 * x4 * t74 * t296)
      t303 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, t213, x2, 0.0D0, x
     #4)
      t309 = t3 * t19
      t310 = t23 * t292
      t323 = -(0.90D2 * t63 * t23 * (t292 * t293 - t301 * t292 * t303) -
     # 0.180D3 * t309 * t310 * t303) * t162 * t93 / 0.720D3 - t63 * t310
     # * t303 * t91 * t169 / 0.8D1
      t324 = FJET(XB1, XB2, s, 0.0D0, -t277 * t278, t277 * t281, -t277 *
     # t283, -s * t285 * t11 * t288 * x4, t323)
      t327 = -0.1D1 + x3
      t329 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #0D0)
      t330 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t331 = -t329 + t330
      t336 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #0D0)
      t337 = t65 * t327
      t340 = log(-0.4D1 * t238 * t337)
      t346 = t85 * t329
      t353 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t354 = t12 * t327
      t355 = x4 * t74
      t359 = log(0.4D1 * t72 * t354 * t355)
      t364 = log(-0.4D1 * t72 * t354 * x4)
      t378 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #0D0)
      t381 = log(-0.4D1 * t64 * t337)
      t383 = t381 ** 2
      t399 = t166 * t331 * t91 * t169 / 0.8D1 - (0.90D2 * t63 * t23 * (t
     #336 - t340 * t329) - 0.180D3 * t3 * t346) * t91 * t162 / 0.720D3 -
     # (0.90D2 * t63 * t23 * (-t353 + t359 * t330 + t336 - t364 * t329) 
     #+ 0.180D3 * t3 * t85 * t331) * t91 * t93 / 0.1440D4 + (-0.90D2 * t
     #63 * t23 * (t378 - t381 * t336 + t383 * t329 / 0.2D1) + 0.180D3 * 
     #t3 * t85 * (t336 - t381 * t329) - t33 * t346) * t91 / 0.1440D4
      t400 = FJET(XB1, XB2, s, t2 * x3, -t2 * t327, 0.0D0, 0.0D0, 0.0D0,
     # t399)
      t402 = KAPPA2(t213, x2, x3, 0.0D0, z)
      t403 = s * t402
      t404 = t278 * x3
      t406 = t278 * t327
      t409 = t402 ** 2
      t415 = 0.1D1 / (-0.2D1 + t402)
      t416 = t23 * t415
      t418 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, t213, x2, x3, 0.0D
     #0)
      t423 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, t213, x2, x3, 0.0D
     #0)
      t426 = t409 ** 2
      t431 = log(-0.4D1 * t173 * t10 * t216 * t327 * t426)
      t445 = -t63 * t416 * t418 * t91 * t169 / 0.8D1 - (0.90D2 * t63 * t
     #23 * (t415 * t423 - t431 * t415 * t418) - 0.180D3 * t309 * t416 * 
     #t418) * t91 * t162 / 0.720D3
      t446 = FJET(XB1, XB2, s, -t403 * t404, t403 * t406, 0.0D0, t403 * 
     #t280, -s * t409 * t11 * t288 * x3, t445)
      t448 = KAPPA2(t213, x2, x3, x4, z)
      t449 = s * t448
      t454 = t448 ** 2
      t459 = cos(t5)
      t462 = Sqrt(x3 * t327 * t355)
      t469 = 0.1D1 / (-0.2D1 + t448)
      t472 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, t213, x2, x3, x4)
      t477 = FJET(XB1, XB2, s, -t449 * t404, t449 * t406, t449 * t281, -
     #t449 * t283, s * t454 * t11 * t288 * (-x3 - x4 + 0.2D1 * x3 * x4 +
     # 0.2D1 * t459 * t462), t63 * t23 * t469 * t472 * t91 * t169 / 0.8D
     #1)
      rrqq2qqht3s4e0 = t208 * t207 + t274 * t273 + t324 * t323 + t400 * 
     #t399 + t446 * t445 + t477 * 0.3141592653589793D1 * t85 * t469 * t4
     #72 * t91 * t162 * t93 / 0.8D1

      end function



      doubleprecision function rrqq2qqht3s4em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 0
     #.0D0)
      t12 = 0.3141592653589793D1 * lh
      t15 = sin(x2 * 0.3141592653589793D1)
      t16 = t15 ** 2
      t17 = z ** 2
      t18 = 0.1D1 / t17
      t19 = t16 * t18
      t20 = t1 ** 2
      t21 = t20 ** 2
      t24 = log(0.4D1 * t19 * t21)
      t25 = t24 * 0.3141592653589793D1
      t29 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.0D0)
      t33 = 0.3141592653589793D1 ** 2
      t35 = lh ** 2
      t41 = t24 ** 2
      t46 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #0.0D0)
      t50 = t4 * t7
      t51 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #x4)
      t52 = -t46 + t51
      t53 = 0.1D1 / x3
      t55 = 0.1D1 / x4
      t59 = x3 * t16
      t60 = t18 * t21
      t63 = log(0.4D1 * t59 * t60)
      t69 = t3 * t7
      t72 = 0.180D3 * t12 * t69 * t46
      t76 = t21 * x4
      t79 = log(0.4D1 * t19 * t76)
      t81 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #x4)
      t82 = -0.1D1 + x4
      t86 = log(-0.4D1 * t19 * t76 * t82)
      t99 = 0.1D1 / x1
      t103 = x1 ** 2
      t104 = t103 * t16
      t107 = log(0.4D1 * t104 * t60)
      t120 = t4 * t7 * t8 / 0.16D2 + (-0.180D3 * t12 - 0.90D2 * t25) * t
     #3 * t7 * t29 / 0.1440D4 + (0.3141592653589793D1 * (-0.30D2 * t33 +
     # 0.180D3 * t35) + 0.180D3 * t25 * lh + 0.45D2 * t41 * 0.3141592653
     #589793D1) * t3 * t7 * t46 / 0.1440D4 - t50 * t52 * t53 * t55 / 0.1
     #6D2 + (0.90D2 * t4 * t7 * (t29 - t63 * t46) - t72) * t53 / 0.1440D
     #4 - (0.90D2 * t4 * t7 * (-t29 + t79 * t46 + t81 - t86 * t51) - 0.1
     #80D3 * t12 * t69 * t52) * t55 / 0.1440D4 + t50 * t46 * t53 * t99 /
     # 0.8D1 + (0.90D2 * t4 * t7 * (t29 - t107 * t46) - t72) * t99 / 0.7
     #20D3 - t50 * t52 * t99 * t55 / 0.8D1
      t121 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t120)
      t123 = -0.1D1 + x1
      t126 = -t123
      t127 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, t126, x2, 0.0D0, 0
     #.0D0)
      t132 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, t126, x2, 0.0D0, 0
     #.0D0)
      t133 = t123 ** 2
      t137 = log(0.4D1 * t104 * t60 * t133)
      t153 = -t50 * t127 * t53 * t99 / 0.8D1 + (-0.90D2 * t4 * t7 * (t13
     #2 - t137 * t127) + 0.180D3 * t12 * t69 * t127) * t99 / 0.720D3 - t
     #50 * t127 * t99 * t55 / 0.8D1
      t154 = FJET(XB1, XB2, s, 0.0D0, -t2 * t123, 0.0D0, t2 * x1, 0.0D0,
     # t153)
      t156 = KAPPA2(t126, x2, 0.0D0, x4, z)
      t157 = s * t156
      t158 = t1 * t123
      t160 = t1 * x1
      t165 = t156 ** 2
      t168 = t123 * x1
      t173 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, t126, x2, 0.0D0, x
     #4)
      t176 = 0.1D1 / (-0.2D1 + t156) * t173 * t99 * t55
      t179 = FJET(XB1, XB2, s, 0.0D0, -t157 * t158, t157 * t160 * x4, -t
     #157 * t160 * t82, -s * t165 * t20 * t168 * x4, -t50 * t176 / 0.8D1
     #)
      t185 = -0.1D1 + x3
      t187 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #0D0)
      t192 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t198 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.
     #0D0)
      t202 = log(-0.4D1 * t59 * t60 * t185)
      t214 = -t50 * t187 * t53 * t99 / 0.8D1 - t50 * (t187 - t192) * t53
     # * t55 / 0.16D2 + (-0.90D2 * t4 * t7 * (t198 - t202 * t187) + 0.18
     #0D3 * t12 * t69 * t187) * t53 / 0.1440D4
      t215 = FJET(XB1, XB2, s, t2 * x3, -t2 * t185, 0.0D0, 0.0D0, 0.0D0,
     # t214)
      t217 = KAPPA2(t126, x2, x3, 0.0D0, z)
      t218 = s * t217
      t224 = t217 ** 2
      t231 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, t126, x2, x3, 0.0D
     #0)
      t234 = 0.1D1 / (-0.2D1 + t217) * t231 * t53 * t99
      t237 = FJET(XB1, XB2, s, -t218 * t158 * x3, t218 * t158 * t185, 0.
     #0D0, t218 * t160, -s * t224 * t20 * t168 * x3, -t50 * t234 / 0.8D1
     #)
      rrqq2qqht3s4em1 = t121 * t120 + t154 * t153 - t179 * 0.31415926535
     #89793D1 * t69 * t176 / 0.8D1 + t215 * t214 - t237 * 0.314159265358
     #9793D1 * t69 * t234 / 0.8D1

      end function



      doubleprecision function rrqq2qqht3s4em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 0
     #.0D0)
      t9 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, x
     #4)
      t16 = t7 * t8
      t17 = 0.1D1 / x3
      t21 = 0.1D1 / x1
      t25 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
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
      t54 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 0.0
     #D0)
      t56 = t7 * t54 * t17
      t59 = FJET(XB1, XB2, s, t2 * x3, -t2 * (-0.1D1 + x3), 0.0D0, 0.0D0
     #, 0.0D0, -t4 * t56 / 0.16D2)
      t64 = -0.1D1 + x1
      t68 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, -t64, x2, 0.0D0, 0.
     #0D0)
      t70 = t7 * t68 * t21
      t73 = FJET(XB1, XB2, s, 0.0D0, -t2 * t64, 0.0D0, t2 * x1, 0.0D0, -
     #t4 * t70 / 0.8D1)
      rrqq2qqht3s4em2 = t49 * t48 - t59 * 0.3141592653589793D1 * t3 * t5
     #6 / 0.16D2 - t73 * 0.3141592653589793D1 * t3 * t70 / 0.8D1

      end function



      doubleprecision function rrqq2qqht3s4em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      t1 = z - 0.1D1
      t3 = 0.1D1 / t1
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 0
     #.0D0)
      t12 = FJET(XB1, XB2, s, 0.0D0, s * t1, 0.0D0, 0.0D0, 0.0D0, 0.3141
     #592653589793D1 * t3 * t7 * t8 / 0.16D2)
      rrqq2qqht3s4em3 = t12 * 0.3141592653589793D1 * t3 * t7 * t8 / 0.16
     #D2

      end function



      doubleprecision function rrqq2qqht3s4em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      rrqq2qqht3s4em4 = 0.0D0

      end function


      doubleprecision function rrqq2qqht3s5e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.3141592653589793D1 ** 2
      t5 = lh ** 2
      t7 = -0.30D2 * t3 + 0.180D3 * t5
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
      t32 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t40 = -0.2884936567583026D3 - 0.120D3 * t5 * lh + 0.60D2 * lh * t3
      t41 = 0.3141592653589793D1 * t40
      t46 = t23 * t19 * 0.3141592653589793D1
      t50 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t54 = t3 ** 2
      t55 = t5 ** 2
      t67 = t23 ** 2
      t72 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t76 = 0.3141592653589793D1 * t27
      t77 = x1 ** 2
      t78 = t77 * t11
      t79 = t13 * t16
      t80 = t79 * x4
      t83 = log(0.4D1 * t78 * t80)
      t85 = t83 ** 2
      t92 = 0.3141592653589793D1 * lh
      t93 = t27 * t31
      t99 = t93 * t72
      t102 = 0.1D1 / x1
      t104 = 0.1D1 / x4
      t107 = t78 * t79
      t109 = log(0.4D1 * t107)
      t115 = t109 ** 2
      t125 = t41 * t99
      t136 = 0.1D1 - x3
      t137 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t136, 0
     #.10D1)
      t138 = x3 * t77
      t139 = t138 * t11
      t140 = -t136
      t141 = t140 * x4
      t145 = log(-0.4D1 * t139 * t79 * t141)
      t146 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t136, 0
     #.10D1)
      t150 = log(0.4D1 * t139 * t80)
      t156 = t72 - t146
      t157 = t93 * t156
      t161 = 0.1D1 / x3
      t163 = t102 * t104
      t166 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t136, 0
     #.10D1)
      t167 = t79 * t140
      t170 = log(-0.4D1 * t139 * t167)
      t172 = t170 ** 2
      t175 = t138 * t17
      t177 = log(0.4D1 * t175)
      t179 = t177 ** 2
      t197 = t16 * x4
      t200 = log(0.4D1 * t14 * t197)
      t206 = t200 ** 2
      t226 = x3 * t11
      t229 = log(0.4D1 * t226 * t80)
      t231 = t229 ** 2
      t234 = t226 * t13
      t235 = t16 * t140
      t239 = log(-0.4D1 * t234 * t235 * x4)
      t241 = t239 ** 2
      t255 = -t93 * t156
      t263 = log(0.4D1 * t226 * t79)
      t267 = log(-0.4D1 * t226 * t167)
      t273 = t267 ** 2
      t280 = t263 ** 2
      t304 = (t8 + 0.180D3 * t20 * lh + 0.45D2 * t24) * t27 * t31 * t32 
     #/ 0.1440D4 + (t41 - t20 * t7 - 0.90D2 * t24 * lh - 0.15D2 * t46) *
     # t27 * t31 * t50 / 0.1440D4 + (0.3141592653589793D1 * (t54 + 0.60D
     #2 * t55 + 0.5769873135166051D3 * lh - 0.60D2 * t5 * t3) - t20 * t4
     #0 + t24 * t7 / 0.2D1 + 0.30D2 * t46 * lh + 0.15D2 / 0.4D1 * t67 * 
     #0.3141592653589793D1) * t27 * t31 * t72 / 0.1440D4 - (0.90D2 * t76
     # * t31 * (-t32 + t83 * t50 - t85 * t72 / 0.2D1) - 0.180D3 * t92 * 
     #t93 * (-t50 + t83 * t72) - t8 * t99) * t102 * t104 / 0.720D3 + (t8
     # * t93 * (t50 - t109 * t72) + 0.90D2 * t76 * t31 * (-t109 * t32 + 
     #t115 * t50 / 0.2D1 - t115 * t109 * t72 / 0.6D1) + t125 - 0.180D3 *
     # t92 * t93 * (t32 - t109 * t50 + t115 * t72 / 0.2D1)) * t102 / 0.7
     #20D3 + (0.90D2 * t76 * t31 * (-t137 + t145 * t146 + t50 - t150 * t
     #72) - 0.180D3 * t92 * t157) * t161 * t163 / 0.720D3 + (0.90D2 * t7
     #6 * t31 * (-t166 + t170 * t137 - t172 * t146 / 0.2D1 + t32 - t177 
     #* t50 + t179 * t72 / 0.2D1) - 0.180D3 * t92 * t93 * (-t137 + t170 
     #* t146 + t50 - t177 * t72) + t8 * t157) * t161 * t102 / 0.720D3 + 
     #(t8 * t93 * (t50 - t200 * t72) + 0.90D2 * t76 * t31 * (-t200 * t32
     # + t206 * t50 / 0.2D1 - t206 * t200 * t72 / 0.6D1) + t125 - 0.180D
     #3 * t92 * t93 * (t32 - t200 * t50 + t206 * t72 / 0.2D1)) * t104 / 
     #0.1440D4 - (0.90D2 * t76 * t31 * (-t32 + t229 * t50 - t231 * t72 /
     # 0.2D1 + t166 - t239 * t137 + t241 * t146 / 0.2D1) - 0.180D3 * t92
     # * t93 * (-t50 + t229 * t72 + t137 - t239 * t146) + t8 * t255) * t
     #161 * t104 / 0.1440D4 - (t8 * t93 * (-t50 + t263 * t72 + t137 - t2
     #67 * t146) + 0.90D2 * t76 * t31 * (-t267 * t166 + t273 * t137 / 0.
     #2D1 - t273 * t267 * t146 / 0.6D1 + t263 * t32 - t280 * t50 / 0.2D1
     # + t280 * t263 * t72 / 0.6D1) + t41 * t255 - 0.180D3 * t92 * t93 *
     # (-t32 + t263 * t50 - t280 * t72 / 0.2D1 + t166 - t267 * t137 + t2
     #73 * t146 / 0.2D1)) * t161 / 0.1440D4
      t305 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t304)
      t307 = -0.1D1 + x4
      t310 = -t307
      t311 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # t310)
      t312 = t197 * t307
      t315 = log(-0.4D1 * t14 * t312)
      t316 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # t310)
      t321 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # t310)
      t323 = t315 ** 2
      t333 = t93 * t316
      t345 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t136, t
     #310)
      t346 = x4 * t307
      t347 = t235 * t346
      t350 = log(0.4D1 * t234 * t347)
      t351 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t136, t
     #310)
      t353 = t350 ** 2
      t354 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t136, t
     #310)
      t359 = log(-0.4D1 * t234 * t312)
      t361 = t359 ** 2
      t374 = -t354 + t316
      t381 = t78 * t13
      t384 = log(-0.4D1 * t381 * t312)
      t386 = t384 ** 2
      t403 = t138 * t14
      t406 = log(0.4D1 * t403 * t347)
      t411 = log(-0.4D1 * t139 * t79 * t346)
      t425 = (-t8 * t93 * (t311 - t315 * t316) - 0.90D2 * t76 * t31 * (-
     #t315 * t321 + t323 * t311 / 0.2D1 - t323 * t315 * t316 / 0.6D1) - 
     #t41 * t333 + 0.180D3 * t92 * t93 * (t321 - t315 * t311 + t323 * t3
     #16 / 0.2D1)) * t104 / 0.1440D4 - (0.90D2 * t76 * t31 * (-t345 + t3
     #50 * t351 - t353 * t354 / 0.2D1 + t321 - t359 * t311 + t361 * t316
     # / 0.2D1) - 0.180D3 * t92 * t93 * (-t351 + t350 * t354 + t311 - t3
     #59 * t316) + t8 * t93 * t374) * t161 * t104 / 0.1440D4 - (0.90D2 *
     # t76 * t31 * (t321 - t384 * t311 + t386 * t316 / 0.2D1) - 0.180D3 
     #* t92 * t93 * (t311 - t384 * t316) + t8 * t333) * t102 * t104 / 0.
     #720D3 + (0.90D2 * t76 * t31 * (t351 - t406 * t354 - t311 + t411 * 
     #t316) + 0.180D3 * t92 * t93 * t374) * t161 * t163 / 0.720D3
      t426 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t307, t2 * x4, 0.0D0,
     # t425)
      t429 = -0.1D1 + x1
      t431 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #10D1)
      t432 = t429 ** 2
      t433 = t16 * t432
      t437 = log(0.4D1 * t381 * t433 * x4)
      t438 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #10D1)
      t440 = t437 ** 2
      t441 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #10D1)
      t453 = t93 * t441
      t454 = t8 * t453
      t458 = t79 * t432
      t461 = log(0.4D1 * t78 * t458)
      t467 = t461 ** 2
      t487 = t432 * x4
      t491 = log(0.4D1 * t139 * t79 * t487)
      t504 = log(0.4D1 * t139 * t458)
      t506 = t504 ** 2
      t522 = -(0.90D2 * t76 * t31 * (t431 - t437 * t438 + t440 * t441 / 
     #0.2D1) - 0.180D3 * t92 * t93 * (t438 - t437 * t441) + t454) * t102
     # * t104 / 0.720D3 + (-t8 * t93 * (t438 - t461 * t441) - 0.90D2 * t
     #76 * t31 * (-t461 * t431 + t467 * t438 / 0.2D1 - t467 * t461 * t44
     #1 / 0.6D1) - t41 * t453 + 0.180D3 * t92 * t93 * (t431 - t461 * t43
     #8 + t467 * t441 / 0.2D1)) * t102 / 0.720D3 + (0.90D2 * t76 * t31 *
     # (-t438 + t491 * t441) + 0.180D3 * t92 * t453) * t161 * t163 / 0.7
     #20D3 + (0.90D2 * t76 * t31 * (-t431 + t504 * t438 - t506 * t441 / 
     #0.2D1) - 0.180D3 * t92 * t93 * (-t438 + t504 * t441) - t454) * t16
     #1 * t102 / 0.720D3
      t523 = FJET(XB1, XB2, s, t2 * x1, 0.0D0, -t2 * t429, 0.0D0, 0.0D0,
     # t522)
      t525 = KAPPA2(x1, x2, 0.10D1, t310, z)
      t526 = s * t525
      t527 = t1 * x1
      t529 = t1 * t429
      t530 = t529 * t307
      t532 = t529 * x4
      t534 = t525 ** 2
      t537 = x1 * t429
      t541 = 0.1D1 / (-0.2D1 + t525)
      t542 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, t3
     #10)
      t544 = t534 ** 2
      t549 = log(-0.4D1 * t107 * t487 * t307 * t544)
      t550 = t549 * t541
      t551 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, t3
     #10)
      t553 = t549 ** 2
      t555 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, t3
     #10)
      t562 = t541 * t551
      t568 = t8 * t27
      t570 = t31 * t541 * t555
      t579 = log(-0.4D1 * t403 * t433 * t346 * t544)
      t586 = t92 * t27
      t593 = -(0.90D2 * t76 * t31 * (t541 * t542 - t550 * t551 + t553 * 
     #t541 * t555 / 0.2D1) - 0.180D3 * t92 * t93 * (t562 - t550 * t555) 
     #+ t568 * t570) * t102 * t104 / 0.720D3 + (0.90D2 * t76 * t31 * (-t
     #562 + t579 * t541 * t555) + 0.180D3 * t586 * t570) * t161 * t163 /
     # 0.720D3
      t594 = FJET(XB1, XB2, s, t526 * t527, 0.0D0, t526 * t530, -t526 * 
     #t532, -s * t534 * t15 * t537 * x4, t593)
      t596 = KAPPA2(x1, x2, t136, 0.10D1, z)
      t597 = s * t596
      t598 = t527 * t140
      t600 = t527 * x3
      t603 = t596 ** 2
      t609 = 0.1D1 / (-0.2D1 + t596)
      t610 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t136, 0.10
     #D1)
      t611 = t609 * t610
      t612 = t603 ** 2
      t617 = log(-0.4D1 * t403 * t433 * t141 * t612)
      t619 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t136, 0.10
     #D1)
      t626 = t31 * t609 * t619
      t632 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, t136, 0.10
     #D1)
      t638 = log(-0.4D1 * t403 * t433 * t140 * t612)
      t639 = t638 * t609
      t641 = t638 ** 2
      t659 = (0.90D2 * t76 * t31 * (-t611 + t617 * t609 * t619) + 0.180D
     #3 * t586 * t626) * t161 * t163 / 0.720D3 + (-0.90D2 * t76 * t31 * 
     #(t609 * t632 - t639 * t610 + t641 * t609 * t619 / 0.2D1) + 0.180D3
     # * t92 * t93 * (t611 - t639 * t619) - t568 * t626) * t161 * t102 /
     # 0.720D3
      t660 = FJET(XB1, XB2, s, -t597 * t598, t597 * t600, -t597 * t529, 
     #0.0D0, -s * t603 * t15 * t537 * x3, t659)
      t662 = KAPPA2(x1, x2, t136, t310, z)
      t663 = s * t662
      t668 = t662 ** 2
      t673 = cos(t9)
      t676 = Sqrt(x3 * t140 * t346)
      t683 = 0.1D1 / (-0.2D1 + t662)
      t684 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t136, t310
     #)
      t687 = t668 ** 2
      t692 = log(0.4D1 * t175 * t432 * t140 * t346 * t687)
      t694 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t136, t310
     #)
      t704 = 0.90D2 * t76 * t31 * (t683 * t684 - t692 * t683 * t694) - 0
     #.180D3 * t586 * t31 * t683 * t694
      t708 = FJET(XB1, XB2, s, -t663 * t598, t663 * t600, t663 * t530, -
     #t663 * t532, s * t668 * t15 * t537 * (-x3 - x4 + 0.2D1 * x3 * x4 +
     # 0.2D1 * t673 * t676), t704 * t161 * t163 / 0.720D3)
      rrqq2qqht3s5e1 = t305 * t304 + t426 * t425 + t523 * t522 + t594 * 
     #t593 + t660 * t659 + t708 * t704 * t161 * t102 * t104 / 0.720D3

      end function



      doubleprecision function rrqq2qqht3s5e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.3141592653589793D1 * lh
      t5 = x2 * 0.3141592653589793D1
      t6 = sin(t5)
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
      t24 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t28 = 0.3141592653589793D1 ** 2
      t30 = lh ** 2
      t32 = -0.30D2 * t28 + 0.180D3 * t30
      t33 = 0.3141592653589793D1 * t32
      t36 = t15 ** 2
      t37 = t36 * 0.3141592653589793D1
      t41 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t59 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t63 = t19 * 0.3141592653589793D1
      t64 = x3 * t7
      t65 = t9 * t12
      t66 = t65 * x4
      t69 = log(0.4D1 * t64 * t66)
      t71 = 0.1D1 - x3
      t72 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t71, 0.1
     #0D1)
      t73 = t64 * t9
      t74 = -t71
      t75 = t12 * t74
      t79 = log(-0.4D1 * t73 * t75 * x4)
      t80 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t71, 0.1
     #0D1)
      t86 = t19 * t23
      t87 = -t59 + t80
      t88 = t86 * t87
      t92 = 0.1D1 / x3
      t94 = 0.1D1 / x4
      t99 = log(0.4D1 * t64 * t65)
      t101 = t99 ** 2
      t104 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t71, 0.
     #10D1)
      t105 = t65 * t74
      t108 = log(-0.4D1 * t64 * t105)
      t110 = t108 ** 2
      t127 = t12 * x4
      t130 = log(0.4D1 * t10 * t127)
      t132 = t130 ** 2
      t144 = t86 * t59
      t145 = t33 * t144
      t149 = x1 ** 2
      t150 = t149 * t7
      t153 = log(0.4D1 * t150 * t66)
      t162 = 0.1D1 / x1
      t166 = t63 * t23
      t167 = -t87
      t169 = t162 * t94
      t173 = x3 * t149
      t174 = t173 * t7
      t177 = log(-0.4D1 * t174 * t105)
      t181 = log(0.4D1 * t173 * t13)
      t194 = t150 * t65
      t196 = log(0.4D1 * t194)
      t198 = t196 ** 2
      t213 = (-0.180D3 * t3 - 0.90D2 * t16) * t19 * t23 * t24 / 0.1440D4
     # + (t33 + 0.180D3 * t16 * lh + 0.45D2 * t37) * t19 * t23 * t41 / 0
     #.1440D4 + (0.3141592653589793D1 * (-0.2884936567583026D3 - 0.120D3
     # * t30 * lh + 0.60D2 * lh * t28) - t16 * t32 - 0.90D2 * t37 * lh -
     # 0.15D2 * t36 * t15 * 0.3141592653589793D1) * t19 * t23 * t59 / 0.
     #1440D4 - (0.90D2 * t63 * t23 * (-t41 + t69 * t59 + t72 - t79 * t80
     #) - 0.180D3 * t3 * t88) * t92 * t94 / 0.1440D4 - (0.90D2 * t63 * t
     #23 * (-t24 + t41 * t99 - t101 * t59 / 0.2D1 + t104 - t108 * t72 + 
     #t110 * t80 / 0.2D1) - 0.180D3 * t3 * t86 * (-t41 + t99 * t59 + t72
     # - t108 * t80) + t33 * t88) * t92 / 0.1440D4 + (0.90D2 * t63 * t23
     # * (t24 - t130 * t41 + t132 * t59 / 0.2D1) - 0.180D3 * t3 * t86 * 
     #(t41 - t130 * t59) + t145) * t94 / 0.1440D4 - (0.90D2 * t63 * t23 
     #* (-t41 + t153 * t59) + 0.180D3 * t3 * t144) * t162 * t94 / 0.720D
     #3 + t166 * t167 * t92 * t169 / 0.8D1 + (0.90D2 * t63 * t23 * (-t72
     # + t177 * t80 + t41 - t181 * t59) - 0.180D3 * t3 * t86 * t167) * t
     #92 * t162 / 0.720D3 + (0.90D2 * t63 * t23 * (t24 - t196 * t41 + t1
     #98 * t59 / 0.2D1) - 0.180D3 * t3 * t86 * (t41 - t196 * t59) + t145
     #) * t162 / 0.720D3
      t214 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t213)
      t216 = -0.1D1 + x4
      t219 = -t216
      t220 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # t219)
      t221 = t127 * t216
      t224 = log(-0.4D1 * t10 * t221)
      t225 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # t219)
      t227 = t224 ** 2
      t228 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # t219)
      t240 = t86 * t228
      t245 = t150 * t9
      t248 = log(-0.4D1 * t245 * t221)
      t260 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t71, t2
     #19)
      t261 = -t228 + t260
      t266 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t71, t2
     #19)
      t267 = x4 * t216
      t271 = log(0.4D1 * t73 * t75 * t267)
      t275 = log(-0.4D1 * t73 * t221)
      t289 = (-0.90D2 * t63 * t23 * (t220 - t224 * t225 + t227 * t228 / 
     #0.2D1) + 0.180D3 * t3 * t86 * (t225 - t224 * t228) - t33 * t240) *
     # t94 / 0.1440D4 - (0.90D2 * t63 * t23 * (t225 - t248 * t228) - 0.1
     #80D3 * t3 * t240) * t162 * t94 / 0.720D3 + t166 * t261 * t92 * t16
     #9 / 0.8D1 - (0.90D2 * t63 * t23 * (-t266 + t271 * t260 + t225 - t2
     #75 * t228) + 0.180D3 * t3 * t86 * t261) * t92 * t94 / 0.1440D4
      t290 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t216, t2 * x4, 0.0D0,
     # t289)
      t293 = -0.1D1 + x1
      t295 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #10D1)
      t296 = t293 ** 2
      t297 = t12 * t296
      t301 = log(0.4D1 * t245 * t297 * x4)
      t302 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #10D1)
      t308 = t86 * t302
      t310 = 0.180D3 * t3 * t308
      t319 = t65 * t296
      t322 = log(0.4D1 * t174 * t319)
      t332 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #10D1)
      t335 = log(0.4D1 * t150 * t319)
      t337 = t335 ** 2
      t353 = -(0.90D2 * t63 * t23 * (t295 - t302 * t301) - t310) * t162 
     #* t94 / 0.720D3 - t166 * t302 * t92 * t169 / 0.8D1 + (0.90D2 * t63
     # * t23 * (-t295 + t322 * t302) + t310) * t92 * t162 / 0.720D3 + (-
     #0.90D2 * t63 * t23 * (t332 - t335 * t295 + t337 * t302 / 0.2D1) + 
     #0.180D3 * t3 * t86 * (t295 - t335 * t302) - t33 * t308) * t162 / 0
     #.720D3
      t354 = FJET(XB1, XB2, s, t2 * x1, 0.0D0, -t2 * t293, 0.0D0, 0.0D0,
     # t353)
      t356 = KAPPA2(x1, x2, 0.10D1, t219, z)
      t357 = s * t356
      t358 = t1 * x1
      t360 = t1 * t293
      t361 = t360 * t216
      t363 = t360 * x4
      t365 = t356 ** 2
      t368 = x1 * t293
      t372 = 0.1D1 / (-0.2D1 + t356)
      t373 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, t2
     #19)
      t376 = t365 ** 2
      t381 = log(-0.4D1 * t194 * t296 * x4 * t216 * t376)
      t383 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, t2
     #19)
      t389 = t3 * t19
      t390 = t23 * t372
      t403 = -(0.90D2 * t63 * t23 * (t372 * t373 - t381 * t372 * t383) -
     # 0.180D3 * t389 * t390 * t383) * t162 * t94 / 0.720D3 - t63 * t390
     # * t383 * t92 * t169 / 0.8D1
      t404 = FJET(XB1, XB2, s, t357 * t358, 0.0D0, t357 * t361, -t357 * 
     #t363, -s * t365 * t11 * t368 * x4, t403)
      t406 = KAPPA2(x1, x2, t71, 0.10D1, z)
      t407 = s * t406
      t408 = t358 * t74
      t410 = t358 * x3
      t413 = t406 ** 2
      t419 = 0.1D1 / (-0.2D1 + t406)
      t420 = t23 * t419
      t422 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t71, 0.10D
     #1)
      t427 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t71, 0.10D
     #1)
      t430 = t413 ** 2
      t435 = log(-0.4D1 * t173 * t10 * t297 * t74 * t430)
      t449 = -t63 * t420 * t422 * t92 * t169 / 0.8D1 + (-0.90D2 * t63 * 
     #t23 * (t419 * t427 - t435 * t419 * t422) + 0.180D3 * t389 * t420 *
     # t422) * t92 * t162 / 0.720D3
      t450 = FJET(XB1, XB2, s, -t407 * t408, t407 * t410, -t407 * t360, 
     #0.0D0, -s * t413 * t11 * t368 * x3, t449)
      t452 = KAPPA2(x1, x2, t71, t219, z)
      t453 = s * t452
      t458 = t452 ** 2
      t463 = cos(t5)
      t466 = Sqrt(x3 * t74 * t267)
      t473 = 0.1D1 / (-0.2D1 + t452)
      t476 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t71, t219)
      t481 = FJET(XB1, XB2, s, -t453 * t408, t453 * t410, t453 * t361, -
     #t453 * t363, s * t458 * t11 * t368 * (-x3 - x4 + 0.2D1 * x3 * x4 +
     # 0.2D1 * t463 * t466), t63 * t23 * t473 * t476 * t92 * t169 / 0.8D
     #1)
      rrqq2qqht3s5e0 = t214 * t213 + t290 * t289 + t353 * t354 + t403 * 
     #t404 + t450 * t449 + t481 * 0.3141592653589793D1 * t86 * t473 * t4
     #76 * t92 * t162 * t94 / 0.8D1

      end function



      doubleprecision function rrqq2qqht3s5em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 0
     #.10D1)
      t12 = 0.3141592653589793D1 * lh
      t15 = sin(x2 * 0.3141592653589793D1)
      t16 = t15 ** 2
      t17 = z ** 2
      t18 = 0.1D1 / t17
      t19 = t16 * t18
      t20 = t1 ** 2
      t21 = t20 ** 2
      t24 = log(0.4D1 * t19 * t21)
      t25 = t24 * 0.3141592653589793D1
      t29 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t33 = 0.3141592653589793D1 ** 2
      t35 = lh ** 2
      t41 = t24 ** 2
      t46 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t50 = t4 * t7
      t51 = 0.1D1 - x3
      t52 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t51, 0.1
     #0D1)
      t53 = -t46 + t52
      t54 = 0.1D1 / x3
      t56 = 0.1D1 / x4
      t60 = x3 * t16
      t61 = t18 * t21
      t64 = log(0.4D1 * t60 * t61)
      t66 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t51, 0.1
     #0D1)
      t67 = -t51
      t71 = log(-0.4D1 * t60 * t61 * t67)
      t77 = t3 * t7
      t84 = t21 * x4
      t87 = log(0.4D1 * t19 * t84)
      t95 = 0.180D3 * t12 * t77 * t46
      t101 = 0.1D1 / x1
      t105 = x1 ** 2
      t106 = t105 * t16
      t109 = log(0.4D1 * t106 * t61)
      t122 = t4 * t7 * t8 / 0.16D2 + (-0.180D3 * t12 - 0.90D2 * t25) * t
     #3 * t7 * t29 / 0.1440D4 + (0.3141592653589793D1 * (-0.30D2 * t33 +
     # 0.180D3 * t35) + 0.180D3 * t25 * lh + 0.45D2 * t41 * 0.3141592653
     #589793D1) * t3 * t7 * t46 / 0.1440D4 - t50 * t53 * t54 * t56 / 0.1
     #6D2 - (0.90D2 * t4 * t7 * (-t29 + t64 * t46 + t66 - t71 * t52) - 0
     #.180D3 * t12 * t77 * t53) * t54 / 0.1440D4 + (0.90D2 * t4 * t7 * (
     #t29 - t87 * t46) - t95) * t56 / 0.1440D4 - t50 * t53 * t54 * t101 
     #/ 0.8D1 + (0.90D2 * t4 * t7 * (t29 - t109 * t46) - t95) * t101 / 0
     #.720D3 + t50 * t46 * t101 * t56 / 0.8D1
      t123 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t122)
      t125 = -0.1D1 + x4
      t128 = -t125
      t129 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # t128)
      t133 = log(-0.4D1 * t19 * t84 * t125)
      t134 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # t128)
      t150 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t51, t1
     #28)
      t156 = (-0.90D2 * t4 * t7 * (t129 - t133 * t134) + 0.180D3 * t12 *
     # t77 * t134) * t56 / 0.1440D4 - t50 * t134 * t101 * t56 / 0.8D1 - 
     #t50 * (-t150 + t134) * t54 * t56 / 0.16D2
      t157 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t125, t2 * x4, 0.0D0,
     # t156)
      t160 = -0.1D1 + x1
      t162 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #10D1)
      t167 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #10D1)
      t168 = t160 ** 2
      t172 = log(0.4D1 * t106 * t61 * t168)
      t188 = -t50 * t162 * t54 * t101 / 0.8D1 + (-0.90D2 * t4 * t7 * (t1
     #67 - t172 * t162) + 0.180D3 * t12 * t77 * t162) * t101 / 0.720D3 -
     # t50 * t162 * t101 * t56 / 0.8D1
      t189 = FJET(XB1, XB2, s, t2 * x1, 0.0D0, -t2 * t160, 0.0D0, 0.0D0,
     # t188)
      t191 = KAPPA2(x1, x2, 0.10D1, t128, z)
      t192 = s * t191
      t193 = t1 * x1
      t195 = t1 * t160
      t200 = t191 ** 2
      t203 = x1 * t160
      t208 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, t1
     #28)
      t211 = 0.1D1 / (-0.2D1 + t191) * t208 * t101 * t56
      t214 = FJET(XB1, XB2, s, t192 * t193, 0.0D0, t192 * t195 * t125, -
     #t192 * t195 * x4, -s * t200 * t20 * t203 * x4, -t50 * t211 / 0.8D1
     #)
      t219 = KAPPA2(x1, x2, t51, 0.10D1, z)
      t220 = s * t219
      t226 = t219 ** 2
      t233 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t51, 0.10D
     #1)
      t236 = 0.1D1 / (-0.2D1 + t219) * t233 * t54 * t101
      t239 = FJET(XB1, XB2, s, -t220 * t193 * t67, t220 * t193 * x3, -t2
     #20 * t195, 0.0D0, -s * t226 * t20 * t203 * x3, -t50 * t236 / 0.8D1
     #)
      rrqq2qqht3s5em1 = t123 * t122 + t157 * t156 + t189 * t188 - t214 *
     # 0.3141592653589793D1 * t77 * t211 / 0.8D1 - t239 * 0.314159265358
     #9793D1 * t77 * t236 / 0.8D1

      end function



      doubleprecision function rrqq2qqht3s5em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 0
     #.10D1)
      t9 = t7 * t8
      t10 = 0.1D1 / x4
      t15 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.1D1 - 
     #x3, 0.10D1)
      t22 = 0.1D1 / x1
      t26 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.10D1)
      t33 = sin(x2 * 0.3141592653589793D1)
      t34 = t33 ** 2
      t35 = z ** 2
      t38 = t1 ** 2
      t39 = t38 ** 2
      t42 = log(0.4D1 * t34 / t35 * t39)
      t49 = t4 * t9 * t10 / 0.16D2 - t4 * t7 * (-t8 + t15) / x3 / 0.16D2
     # + t4 * t9 * t22 / 0.8D1 + t4 * t7 * t26 / 0.16D2 + (-0.180D3 * 0.
     #3141592653589793D1 * lh - 0.90D2 * t42 * 0.3141592653589793D1) * t
     #3 * t9 / 0.1440D4
      t50 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t49)
      t52 = -0.1D1 + x4
      t56 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #-t52)
      t58 = t7 * t56 * t10
      t61 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t52, t2 * x4, 0.0D0, -
     #t4 * t58 / 0.16D2)
      t69 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.1
     #0D1)
      t71 = t7 * t69 * t22
      t74 = FJET(XB1, XB2, s, t2 * x1, 0.0D0, -t2 * (-0.1D1 + x1), 0.0D0
     #, 0.0D0, -t4 * t71 / 0.8D1)
      rrqq2qqht3s5em2 = t50 * t49 - t61 * 0.3141592653589793D1 * t3 * t5
     #8 / 0.16D2 - t74 * 0.3141592653589793D1 * t3 * t71 / 0.8D1

      end function



      doubleprecision function rrqq2qqht3s5em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      t1 = z - 0.1D1
      t3 = 0.1D1 / t1
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 0
     #.10D1)
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, s * t1, 0.0D0, 0.0D0, 0.3141
     #592653589793D1 * t3 * t7 * t8 / 0.16D2)
      rrqq2qqht3s5em3 = t12 * 0.3141592653589793D1 * t3 * t7 * t8 / 0.16
     #D2

      end function



      doubleprecision function rrqq2qqht3s5em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      rrqq2qqht3s5em4 = 0.0D0

      end function


      doubleprecision function rrqq2qqht3s6e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.3141592653589793D1 ** 2
      t5 = lh ** 2
      t7 = -0.30D2 * t3 + 0.180D3 * t5
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
      t32 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t40 = -0.2884936567583026D3 - 0.120D3 * t5 * lh + 0.60D2 * lh * t3
      t41 = 0.3141592653589793D1 * t40
      t46 = t23 * t19 * 0.3141592653589793D1
      t50 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t54 = t3 ** 2
      t55 = t5 ** 2
      t67 = t23 ** 2
      t72 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t76 = 0.3141592653589793D1 * t27
      t77 = x1 ** 2
      t78 = t77 * t11
      t79 = t13 * t16
      t80 = t79 * x4
      t83 = log(0.4D1 * t78 * t80)
      t85 = t83 ** 2
      t92 = 0.3141592653589793D1 * lh
      t93 = t27 * t31
      t99 = t93 * t72
      t102 = 0.1D1 / x1
      t104 = 0.1D1 / x4
      t107 = t78 * t79
      t109 = log(0.4D1 * t107)
      t115 = t109 ** 2
      t125 = t41 * t99
      t136 = 0.1D1 - x3
      t137 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t136, 0
     #.0D0)
      t138 = x3 * t77
      t139 = t138 * t11
      t140 = -t136
      t141 = t140 * x4
      t145 = log(-0.4D1 * t139 * t79 * t141)
      t146 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t136, 0
     #.0D0)
      t150 = log(0.4D1 * t139 * t80)
      t156 = t72 - t146
      t157 = t93 * t156
      t161 = 0.1D1 / x3
      t163 = t102 * t104
      t166 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t136, 0
     #.0D0)
      t167 = t79 * t140
      t170 = log(-0.4D1 * t139 * t167)
      t172 = t170 ** 2
      t175 = t138 * t17
      t177 = log(0.4D1 * t175)
      t179 = t177 ** 2
      t194 = -t8 * t93 * t156
      t199 = t16 * x4
      t202 = log(0.4D1 * t14 * t199)
      t208 = t202 ** 2
      t228 = x3 * t11
      t231 = log(0.4D1 * t228 * t80)
      t233 = t231 ** 2
      t236 = t228 * t13
      t237 = t16 * t140
      t241 = log(-0.4D1 * t236 * t237 * x4)
      t243 = t241 ** 2
      t262 = log(0.4D1 * t228 * t79)
      t266 = log(-0.4D1 * t228 * t167)
      t272 = t266 ** 2
      t279 = t262 ** 2
      t303 = (t8 + 0.180D3 * t20 * lh + 0.45D2 * t24) * t27 * t31 * t32 
     #/ 0.1440D4 + (t41 - t20 * t7 - 0.90D2 * t24 * lh - 0.15D2 * t46) *
     # t27 * t31 * t50 / 0.1440D4 + (0.3141592653589793D1 * (t54 + 0.60D
     #2 * t55 + 0.5769873135166051D3 * lh - 0.60D2 * t5 * t3) - t20 * t4
     #0 + t24 * t7 / 0.2D1 + 0.30D2 * t46 * lh + 0.15D2 / 0.4D1 * t67 * 
     #0.3141592653589793D1) * t27 * t31 * t72 / 0.1440D4 + (0.90D2 * t76
     # * t31 * (t32 - t83 * t50 + t85 * t72 / 0.2D1) - 0.180D3 * t92 * t
     #93 * (t50 - t83 * t72) + t8 * t99) * t102 * t104 / 0.720D3 - (t8 *
     # t93 * (-t50 + t109 * t72) + 0.90D2 * t76 * t31 * (t109 * t32 - t1
     #15 * t50 / 0.2D1 + t115 * t109 * t72 / 0.6D1) - t125 - 0.180D3 * t
     #92 * t93 * (-t32 + t109 * t50 - t115 * t72 / 0.2D1)) * t102 / 0.72
     #0D3 + (0.90D2 * t76 * t31 * (-t137 + t145 * t146 + t50 - t150 * t7
     #2) - 0.180D3 * t92 * t157) * t161 * t163 / 0.720D3 - (0.90D2 * t76
     # * t31 * (t166 - t170 * t137 + t172 * t146 / 0.2D1 - t32 + t177 * 
     #t50 - t179 * t72 / 0.2D1) - 0.180D3 * t92 * t93 * (t137 - t170 * t
     #146 - t50 + t177 * t72) + t194) * t161 * t102 / 0.720D3 - (t8 * t9
     #3 * (-t50 + t202 * t72) + 0.90D2 * t76 * t31 * (t202 * t32 - t208 
     #* t50 / 0.2D1 + t208 * t202 * t72 / 0.6D1) - t125 - 0.180D3 * t92 
     #* t93 * (-t32 + t202 * t50 - t208 * t72 / 0.2D1)) * t104 / 0.1440D
     #4 - (0.90D2 * t76 * t31 * (-t32 + t231 * t50 - t233 * t72 / 0.2D1 
     #+ t166 - t241 * t137 + t243 * t146 / 0.2D1) - 0.180D3 * t92 * t93 
     #* (-t50 + t231 * t72 + t137 - t241 * t146) + t194) * t161 * t104 /
     # 0.1440D4 + (t8 * t93 * (t50 - t262 * t72 - t137 + t266 * t146) + 
     #0.90D2 * t76 * t31 * (t266 * t166 - t272 * t137 / 0.2D1 + t272 * t
     #266 * t146 / 0.6D1 - t262 * t32 + t279 * t50 / 0.2D1 - t279 * t262
     # * t72 / 0.6D1) + t41 * t157 - 0.180D3 * t92 * t93 * (t32 - t262 *
     # t50 + t279 * t72 / 0.2D1 - t166 + t266 * t137 - t272 * t146 / 0.2
     #D1)) * t161 / 0.1440D4
      t304 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t303)
      t307 = -0.1D1 + x4
      t309 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t310 = t199 * t307
      t313 = log(-0.4D1 * t14 * t310)
      t314 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t319 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t321 = t313 ** 2
      t331 = t93 * t314
      t345 = log(-0.4D1 * t236 * t310)
      t347 = t345 ** 2
      t350 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t136, x
     #4)
      t351 = x4 * t307
      t352 = t237 * t351
      t355 = log(0.4D1 * t236 * t352)
      t356 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t136, x
     #4)
      t358 = t355 ** 2
      t359 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t136, x
     #4)
      t372 = t314 - t359
      t379 = t78 * t13
      t382 = log(-0.4D1 * t379 * t310)
      t384 = t382 ** 2
      t404 = log(-0.4D1 * t139 * t79 * t351)
      t406 = t138 * t14
      t409 = log(0.4D1 * t406 * t352)
      t423 = -(t8 * t93 * (t309 - t313 * t314) + 0.90D2 * t76 * t31 * (-
     #t313 * t319 + t321 * t309 / 0.2D1 - t321 * t313 * t314 / 0.6D1) + 
     #t41 * t331 - 0.180D3 * t92 * t93 * (t319 - t313 * t309 + t321 * t3
     #14 / 0.2D1)) * t104 / 0.1440D4 - (0.90D2 * t76 * t31 * (t319 - t34
     #5 * t309 + t347 * t314 / 0.2D1 - t350 + t355 * t356 - t358 * t359 
     #/ 0.2D1) - 0.180D3 * t92 * t93 * (t309 - t345 * t314 - t356 + t355
     # * t359) + t8 * t93 * t372) * t161 * t104 / 0.1440D4 + (0.90D2 * t
     #76 * t31 * (-t319 + t382 * t309 - t384 * t314 / 0.2D1) - 0.180D3 *
     # t92 * t93 * (-t309 + t382 * t314) - t8 * t331) * t102 * t104 / 0.
     #720D3 + (0.90D2 * t76 * t31 * (-t309 + t404 * t314 + t356 - t409 *
     # t359) + 0.180D3 * t92 * t93 * t372) * t161 * t163 / 0.720D3
      t424 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * t307, 0.0D0,
     # t423)
      t426 = KAPPA2(x1, x2, 0.10D1, 0.0D0, z)
      t427 = s * t426
      t428 = t1 * x1
      t430 = -0.1D1 + x1
      t431 = t1 * t430
      t433 = t426 ** 2
      t439 = 0.1D1 / (-0.2D1 + t426)
      t440 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #0D0)
      t441 = t439 * t440
      t442 = t430 ** 2
      t443 = t16 * t442
      t444 = t433 ** 2
      t446 = t443 * x4 * t444
      t449 = log(0.4D1 * t379 * t446)
      t450 = t449 * t439
      t451 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #0D0)
      t453 = t449 ** 2
      t455 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #0D0)
      t462 = t439 * t451
      t468 = t8 * t27
      t470 = t31 * t439 * t455
      t471 = t468 * t470
      t478 = log(0.4D1 * t379 * t443 * t444)
      t479 = t478 * t439
      t485 = t478 ** 2
      t486 = t485 * t439
      t510 = log(0.4D1 * t406 * t446)
      t517 = t92 * t27
      t527 = log(0.4D1 * t139 * t79 * t442 * t444)
      t528 = t527 * t439
      t530 = t527 ** 2
      t547 = (0.90D2 * t76 * t31 * (t441 - t450 * t451 + t453 * t439 * t
     #455 / 0.2D1) - 0.180D3 * t92 * t93 * (t462 - t450 * t455) + t471) 
     #* t102 * t104 / 0.720D3 - (-t8 * t93 * (t462 - t479 * t455) - 0.90
     #D2 * t76 * t31 * (-t479 * t440 + t486 * t451 / 0.2D1 - t485 * t478
     # * t439 * t455 / 0.6D1) - t41 * t27 * t470 + 0.180D3 * t92 * t93 *
     # (t441 - t479 * t451 + t486 * t455 / 0.2D1)) * t102 / 0.720D3 + (0
     #.90D2 * t76 * t31 * (t462 - t510 * t439 * t455) - 0.180D3 * t517 *
     # t470) * t161 * t163 / 0.720D3 - (0.90D2 * t76 * t31 * (-t441 + t5
     #28 * t451 - t530 * t439 * t455 / 0.2D1) - 0.180D3 * t92 * t93 * (-
     #t462 + t528 * t455) - t471) * t161 * t102 / 0.720D3
      t548 = FJET(XB1, XB2, s, t427 * t428, 0.0D0, 0.0D0, -t427 * t431, 
     #-s * t433 * t15 * x1 * t430, t547)
      t550 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t551 = s * t550
      t553 = t431 * x4
      t555 = t431 * t307
      t557 = t550 ** 2
      t560 = x1 * t430
      t564 = 0.1D1 / (-0.2D1 + t550)
      t565 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t568 = t557 ** 2
      t573 = log(-0.4D1 * t107 * t442 * x4 * t307 * t568)
      t574 = t573 * t564
      t575 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t577 = t573 ** 2
      t579 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t586 = t564 * t575
      t593 = t31 * t564 * t579
      t602 = log(-0.4D1 * t406 * t443 * t351 * t568)
      t615 = (-0.90D2 * t76 * t31 * (t564 * t565 - t574 * t575 + t577 * 
     #t564 * t579 / 0.2D1) + 0.180D3 * t92 * t93 * (t586 - t574 * t579) 
     #- t468 * t593) * t102 * t104 / 0.720D3 + (0.90D2 * t76 * t31 * (-t
     #586 + t602 * t564 * t579) + 0.180D3 * t517 * t593) * t161 * t163 /
     # 0.720D3
      t616 = FJET(XB1, XB2, s, t551 * t428, 0.0D0, -t551 * t553, t551 * 
     #t555, s * t557 * t15 * t560 * t307, t615)
      t618 = KAPPA2(x1, x2, t136, 0.0D0, z)
      t619 = s * t618
      t620 = t428 * t140
      t622 = t428 * x3
      t625 = t618 ** 2
      t631 = 0.1D1 / (-0.2D1 + t618)
      t632 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t136, 0.0D
     #0)
      t633 = t631 * t632
      t634 = t625 ** 2
      t639 = log(-0.4D1 * t406 * t443 * t141 * t634)
      t641 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t136, 0.0D
     #0)
      t648 = t31 * t631 * t641
      t654 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, t136, 0.0D
     #0)
      t660 = log(-0.4D1 * t406 * t443 * t140 * t634)
      t661 = t660 * t631
      t663 = t660 ** 2
      t681 = (0.90D2 * t76 * t31 * (-t633 + t639 * t631 * t641) + 0.180D
     #3 * t517 * t648) * t161 * t163 / 0.720D3 - (0.90D2 * t76 * t31 * (
     #t631 * t654 - t661 * t632 + t663 * t631 * t641 / 0.2D1) - 0.180D3 
     #* t92 * t93 * (t633 - t661 * t641) + t468 * t648) * t161 * t102 / 
     #0.720D3
      t682 = FJET(XB1, XB2, s, -t619 * t620, t619 * t622, 0.0D0, -t619 *
     # t431, s * t625 * t15 * t560 * t140, t681)
      t684 = KAPPA2(x1, x2, t136, x4, z)
      t685 = s * t684
      t690 = t684 ** 2
      t695 = cos(t9)
      t698 = Sqrt(x3 * t140 * t351)
      t705 = 0.1D1 / (-0.2D1 + t684)
      t706 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t136, x4)
      t709 = t690 ** 2
      t714 = log(0.4D1 * t175 * t442 * t140 * t351 * t709)
      t716 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t136, x4)
      t726 = 0.90D2 * t76 * t31 * (t705 * t706 - t714 * t705 * t716) - 0
     #.180D3 * t517 * t31 * t705 * t716
      t730 = FJET(XB1, XB2, s, -t685 * t620, t685 * t622, -t685 * t553, 
     #t685 * t555, s * t690 * t15 * t560 * (-0.1D1 + x3 + x4 - 0.2D1 * x
     #3 * x4 + 0.2D1 * t695 * t698), t726 * t161 * t163 / 0.720D3)
      rrqq2qqht3s6e1 = t304 * t303 + t424 * t423 + t548 * t547 + t616 * 
     #t615 + t682 * t681 + t730 * t726 * t161 * t102 * t104 / 0.720D3

      end function



      doubleprecision function rrqq2qqht3s6e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.3141592653589793D1 * lh
      t5 = x2 * 0.3141592653589793D1
      t6 = sin(t5)
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
      t24 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t28 = 0.3141592653589793D1 ** 2
      t30 = lh ** 2
      t32 = -0.30D2 * t28 + 0.180D3 * t30
      t33 = 0.3141592653589793D1 * t32
      t36 = t15 ** 2
      t37 = t36 * 0.3141592653589793D1
      t41 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t59 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t63 = t19 * 0.3141592653589793D1
      t64 = x3 * t7
      t65 = t9 * t12
      t66 = t65 * x4
      t69 = log(0.4D1 * t64 * t66)
      t71 = 0.1D1 - x3
      t72 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t71, 0.0
     #D0)
      t73 = t64 * t9
      t74 = -t71
      t75 = t12 * t74
      t79 = log(-0.4D1 * t73 * t75 * x4)
      t80 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t71, 0.0
     #D0)
      t86 = t19 * t23
      t87 = -t59 + t80
      t90 = 0.180D3 * t3 * t86 * t87
      t92 = 0.1D1 / x3
      t94 = 0.1D1 / x4
      t99 = log(0.4D1 * t64 * t65)
      t101 = t99 ** 2
      t104 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t71, 0.
     #0D0)
      t105 = t65 * t74
      t108 = log(-0.4D1 * t64 * t105)
      t110 = t108 ** 2
      t123 = -t87
      t129 = t12 * x4
      t132 = log(0.4D1 * t10 * t129)
      t134 = t132 ** 2
      t146 = t86 * t59
      t147 = t33 * t146
      t151 = x1 ** 2
      t152 = t151 * t7
      t155 = log(0.4D1 * t152 * t66)
      t164 = 0.1D1 / x1
      t168 = t63 * t23
      t170 = t164 * t94
      t174 = x3 * t151
      t175 = t174 * t7
      t178 = log(-0.4D1 * t175 * t105)
      t182 = log(0.4D1 * t174 * t13)
      t192 = t152 * t65
      t194 = log(0.4D1 * t192)
      t196 = t194 ** 2
      t211 = (-0.180D3 * t3 - 0.90D2 * t16) * t19 * t23 * t24 / 0.1440D4
     # + (t33 + 0.180D3 * t16 * lh + 0.45D2 * t37) * t19 * t23 * t41 / 0
     #.1440D4 + (0.3141592653589793D1 * (-0.2884936567583026D3 - 0.120D3
     # * t30 * lh + 0.60D2 * lh * t28) - t16 * t32 - 0.90D2 * t37 * lh -
     # 0.15D2 * t36 * t15 * 0.3141592653589793D1) * t19 * t23 * t59 / 0.
     #1440D4 - (0.90D2 * t63 * t23 * (-t41 + t69 * t59 + t72 - t79 * t80
     #) - t90) * t92 * t94 / 0.1440D4 + (0.90D2 * t63 * t23 * (t24 - t41
     # * t99 + t101 * t59 / 0.2D1 - t104 + t108 * t72 - t110 * t80 / 0.2
     #D1) - 0.180D3 * t3 * t86 * (t41 - t99 * t59 - t72 + t108 * t80) + 
     #t33 * t86 * t123) * t92 / 0.1440D4 - (0.90D2 * t63 * t23 * (-t24 +
     # t132 * t41 - t134 * t59 / 0.2D1) - 0.180D3 * t3 * t86 * (-t41 + t
     #132 * t59) - t147) * t94 / 0.1440D4 + (0.90D2 * t63 * t23 * (t41 -
     # t155 * t59) - 0.180D3 * t3 * t146) * t164 * t94 / 0.720D3 + t168 
     #* t123 * t92 * t170 / 0.8D1 - (0.90D2 * t63 * t23 * (t72 - t178 * 
     #t80 - t41 + t182 * t59) - t90) * t92 * t164 / 0.720D3 - (0.90D2 * 
     #t63 * t23 * (-t24 + t194 * t41 - t196 * t59 / 0.2D1) - 0.180D3 * t
     #3 * t86 * (-t41 + t194 * t59) - t147) * t164 / 0.720D3
      t212 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t211)
      t215 = -0.1D1 + x4
      t217 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t218 = t129 * t215
      t221 = log(-0.4D1 * t10 * t218)
      t222 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t224 = t221 ** 2
      t225 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t237 = t86 * t225
      t242 = t152 * t9
      t245 = log(-0.4D1 * t242 * t218)
      t257 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t71, x4
     #)
      t258 = -t225 + t257
      t265 = log(-0.4D1 * t73 * t218)
      t267 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t71, x4
     #)
      t268 = x4 * t215
      t272 = log(0.4D1 * t73 * t75 * t268)
      t286 = -(0.90D2 * t63 * t23 * (t217 - t221 * t222 + t224 * t225 / 
     #0.2D1) - 0.180D3 * t3 * t86 * (t222 - t221 * t225) + t33 * t237) *
     # t94 / 0.1440D4 + (0.90D2 * t63 * t23 * (-t222 + t245 * t225) + 0.
     #180D3 * t3 * t237) * t164 * t94 / 0.720D3 + t168 * t258 * t92 * t1
     #70 / 0.8D1 - (0.90D2 * t63 * t23 * (t222 - t265 * t225 - t267 + t2
     #72 * t257) + 0.180D3 * t3 * t86 * t258) * t92 * t94 / 0.1440D4
      t287 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * t215, 0.0D0,
     # t286)
      t289 = KAPPA2(x1, x2, 0.10D1, 0.0D0, z)
      t290 = s * t289
      t291 = t1 * x1
      t293 = -0.1D1 + x1
      t294 = t1 * t293
      t296 = t289 ** 2
      t302 = 0.1D1 / (-0.2D1 + t289)
      t303 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #0D0)
      t304 = t302 * t303
      t305 = t293 ** 2
      t306 = t12 * t305
      t307 = t296 ** 2
      t312 = log(0.4D1 * t242 * t306 * x4 * t307)
      t314 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #0D0)
      t320 = t3 * t19
      t321 = t23 * t302
      t322 = t321 * t314
      t324 = 0.180D3 * t320 * t322
      t338 = log(0.4D1 * t175 * t65 * t305 * t307)
      t349 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #0D0)
      t354 = log(0.4D1 * t242 * t306 * t307)
      t355 = t354 * t302
      t357 = t354 ** 2
      t375 = (0.90D2 * t63 * t23 * (t304 - t312 * t302 * t314) - t324) *
     # t164 * t94 / 0.720D3 + t63 * t321 * t314 * t92 * t170 / 0.8D1 - (
     #0.90D2 * t63 * t23 * (-t304 + t338 * t302 * t314) + t324) * t92 * 
     #t164 / 0.720D3 - (-0.90D2 * t63 * t23 * (t302 * t349 - t355 * t303
     # + t357 * t302 * t314 / 0.2D1) + 0.180D3 * t3 * t86 * (t304 - t355
     # * t314) - t33 * t19 * t322) * t164 / 0.720D3
      t376 = FJET(XB1, XB2, s, t290 * t291, 0.0D0, 0.0D0, -t290 * t294, 
     #-s * t296 * t11 * x1 * t293, t375)
      t378 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t379 = s * t378
      t381 = t294 * x4
      t383 = t294 * t215
      t385 = t378 ** 2
      t388 = x1 * t293
      t392 = 0.1D1 / (-0.2D1 + t378)
      t393 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t396 = t385 ** 2
      t401 = log(-0.4D1 * t192 * t305 * x4 * t215 * t396)
      t403 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t409 = t23 * t392
      t422 = (-0.90D2 * t63 * t23 * (t392 * t393 - t401 * t392 * t403) +
     # 0.180D3 * t320 * t409 * t403) * t164 * t94 / 0.720D3 - t63 * t409
     # * t403 * t92 * t170 / 0.8D1
      t423 = FJET(XB1, XB2, s, t379 * t291, 0.0D0, -t379 * t381, t379 * 
     #t383, s * t385 * t11 * t388 * t215, t422)
      t425 = KAPPA2(x1, x2, t71, 0.0D0, z)
      t426 = s * t425
      t427 = t291 * t74
      t429 = t291 * x3
      t432 = t425 ** 2
      t438 = 0.1D1 / (-0.2D1 + t425)
      t439 = t23 * t438
      t441 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t71, 0.0D0
     #)
      t446 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t71, 0.0D0
     #)
      t449 = t432 ** 2
      t454 = log(-0.4D1 * t174 * t10 * t306 * t74 * t449)
      t468 = -t63 * t439 * t441 * t92 * t170 / 0.8D1 - (0.90D2 * t63 * t
     #23 * (t438 * t446 - t454 * t438 * t441) - 0.180D3 * t320 * t439 * 
     #t441) * t92 * t164 / 0.720D3
      t469 = FJET(XB1, XB2, s, -t426 * t427, t426 * t429, 0.0D0, -t426 *
     # t294, s * t432 * t11 * t388 * t74, t468)
      t471 = KAPPA2(x1, x2, t71, x4, z)
      t472 = s * t471
      t477 = t471 ** 2
      t482 = cos(t5)
      t485 = Sqrt(x3 * t74 * t268)
      t492 = 0.1D1 / (-0.2D1 + t471)
      t495 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t71, x4)
      t500 = FJET(XB1, XB2, s, -t472 * t427, t472 * t429, -t472 * t381, 
     #t472 * t383, s * t477 * t11 * t388 * (-0.1D1 + x3 + x4 - 0.2D1 * x
     #3 * x4 + 0.2D1 * t482 * t485), t63 * t23 * t492 * t495 * t92 * t17
     #0 / 0.8D1)
      rrqq2qqht3s6e0 = t212 * t211 + t287 * t286 + t376 * t375 + t423 * 
     #t422 + t469 * t468 + t500 * 0.3141592653589793D1 * t86 * t492 * t4
     #95 * t92 * t164 * t94 / 0.8D1

      end function



      doubleprecision function rrqq2qqht3s6em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 0
     #.0D0)
      t12 = 0.3141592653589793D1 * lh
      t15 = sin(x2 * 0.3141592653589793D1)
      t16 = t15 ** 2
      t17 = z ** 2
      t18 = 0.1D1 / t17
      t19 = t16 * t18
      t20 = t1 ** 2
      t21 = t20 ** 2
      t24 = log(0.4D1 * t19 * t21)
      t25 = t24 * 0.3141592653589793D1
      t29 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t33 = 0.3141592653589793D1 ** 2
      t35 = lh ** 2
      t41 = t24 ** 2
      t46 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t50 = t4 * t7
      t51 = 0.1D1 - x3
      t52 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t51, 0.0
     #D0)
      t53 = -t46 + t52
      t54 = 0.1D1 / x3
      t55 = t53 * t54
      t56 = 0.1D1 / x4
      t60 = x3 * t16
      t61 = t18 * t21
      t64 = log(0.4D1 * t60 * t61)
      t66 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t51, 0.0
     #D0)
      t67 = -t51
      t71 = log(-0.4D1 * t60 * t61 * t67)
      t77 = t3 * t7
      t85 = t21 * x4
      t88 = log(0.4D1 * t19 * t85)
      t96 = 0.180D3 * t12 * t77 * t46
      t100 = 0.1D1 / x1
      t104 = x1 ** 2
      t105 = t104 * t16
      t108 = log(0.4D1 * t105 * t61)
      t121 = t4 * t7 * t8 / 0.16D2 + (-0.180D3 * t12 - 0.90D2 * t25) * t
     #3 * t7 * t29 / 0.1440D4 + (0.3141592653589793D1 * (-0.30D2 * t33 +
     # 0.180D3 * t35) + 0.180D3 * t25 * lh + 0.45D2 * t41 * 0.3141592653
     #589793D1) * t3 * t7 * t46 / 0.1440D4 - t50 * t55 * t56 / 0.16D2 + 
     #(0.90D2 * t4 * t7 * (t29 - t64 * t46 - t66 + t71 * t52) + 0.180D3 
     #* t12 * t77 * t53) * t54 / 0.1440D4 - (0.90D2 * t4 * t7 * (-t29 + 
     #t88 * t46) + t96) * t56 / 0.1440D4 - t50 * t55 * t100 / 0.8D1 - (0
     #.90D2 * t4 * t7 * (-t29 + t108 * t46) + t96) * t100 / 0.720D3 + t5
     #0 * t46 * t100 * t56 / 0.8D1
      t122 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t121)
      t125 = -0.1D1 + x4
      t127 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t131 = log(-0.4D1 * t19 * t85 * t125)
      t132 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t148 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t51, x4
     #)
      t154 = -(0.90D2 * t4 * t7 * (t127 - t131 * t132) - 0.180D3 * t12 *
     # t77 * t132) * t56 / 0.1440D4 - t50 * t132 * t100 * t56 / 0.8D1 - 
     #t50 * (t132 - t148) * t54 * t56 / 0.16D2
      t155 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * t125, 0.0D0,
     # t154)
      t157 = KAPPA2(x1, x2, 0.10D1, 0.0D0, z)
      t158 = s * t157
      t159 = t1 * x1
      t161 = -0.1D1 + x1
      t162 = t1 * t161
      t164 = t157 ** 2
      t170 = 0.1D1 / (-0.2D1 + t157)
      t171 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #0D0)
      t172 = t170 * t171
      t173 = t54 * t100
      t177 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.
     #0D0)
      t180 = t161 ** 2
      t182 = t164 ** 2
      t186 = log(0.4D1 * t105 * t18 * t21 * t180 * t182)
      t201 = t100 * t56
      t205 = t50 * t172 * t173 / 0.8D1 - (-0.90D2 * t4 * t7 * (t170 * t1
     #77 - t186 * t170 * t171) + 0.180D3 * t12 * t3 * t7 * t170 * t171) 
     #* t100 / 0.720D3 + t50 * t172 * t201 / 0.8D1
      t206 = FJET(XB1, XB2, s, t158 * t159, 0.0D0, 0.0D0, -t158 * t162, 
     #-s * t164 * t20 * x1 * t161, t205)
      t208 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t209 = s * t208
      t215 = t208 ** 2
      t218 = x1 * t161
      t223 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t225 = 0.1D1 / (-0.2D1 + t208) * t223 * t201
      t228 = FJET(XB1, XB2, s, t209 * t159, 0.0D0, -t209 * t162 * x4, t2
     #09 * t162 * t125, s * t215 * t20 * t218 * t125, -t50 * t225 / 0.8D
     #1)
      t233 = KAPPA2(x1, x2, t51, 0.0D0, z)
      t234 = s * t233
      t240 = t233 ** 2
      t247 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t51, 0.0D0
     #)
      t249 = 0.1D1 / (-0.2D1 + t233) * t247 * t173
      t252 = FJET(XB1, XB2, s, -t234 * t159 * t67, t234 * t159 * x3, 0.0
     #D0, -t234 * t162, s * t240 * t20 * t218 * t67, -t50 * t249 / 0.8D1
     #)
      rrqq2qqht3s6em1 = t122 * t121 + t155 * t154 + t206 * t205 - t228 *
     # 0.3141592653589793D1 * t77 * t225 / 0.8D1 - t252 * 0.314159265358
     #9793D1 * t77 * t249 / 0.8D1

      end function



      doubleprecision function rrqq2qqht3s6em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 0
     #.0D0)
      t9 = t7 * t8
      t10 = 0.1D1 / x4
      t15 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.1D1 - 
     #x3, 0.0D0)
      t22 = 0.1D1 / x1
      t26 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #0.0D0)
      t33 = sin(x2 * 0.3141592653589793D1)
      t34 = t33 ** 2
      t35 = z ** 2
      t38 = t1 ** 2
      t39 = t38 ** 2
      t42 = log(0.4D1 * t34 / t35 * t39)
      t49 = t4 * t9 * t10 / 0.16D2 + t4 * t7 * (t8 - t15) / x3 / 0.16D2 
     #+ t4 * t9 * t22 / 0.8D1 + t4 * t7 * t26 / 0.16D2 + (-0.180D3 * 0.3
     #141592653589793D1 * lh - 0.90D2 * t42 * 0.3141592653589793D1) * t3
     # * t9 / 0.1440D4
      t50 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t49)
      t55 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #x4)
      t57 = t7 * t55 * t10
      t60 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * (-0.1D1 + x4)
     #, 0.0D0, -t4 * t57 / 0.16D2)
      t65 = KAPPA2(x1, x2, 0.10D1, 0.0D0, z)
      t66 = s * t65
      t69 = -0.1D1 + x1
      t72 = t65 ** 2
      t79 = 0.1D1 / (-0.2D1 + t65)
      t80 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 0.0
     #D0)
      t85 = FJET(XB1, XB2, s, t66 * t1 * x1, 0.0D0, 0.0D0, -t66 * t1 * t
     #69, -s * t72 * t38 * x1 * t69, t4 * t7 * t79 * t80 * t22 / 0.8D1)
      rrqq2qqht3s6em2 = t50 * t49 - t60 * 0.3141592653589793D1 * t3 * t5
     #7 / 0.16D2 + t85 * 0.3141592653589793D1 * t3 * t7 * t79 * t80 * t2
     #2 / 0.8D1

      end function



      doubleprecision function rrqq2qqht3s6em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      t1 = z - 0.1D1
      t3 = 0.1D1 / t1
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 0
     #.0D0)
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, s * t1, 0.0D0, 0.3141
     #592653589793D1 * t3 * t7 * t8 / 0.16D2)
      rrqq2qqht3s6em3 = t12 * 0.3141592653589793D1 * t3 * t7 * t8 / 0.16
     #D2

      end function



      doubleprecision function rrqq2qqht3s6em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      rrqq2qqht3s6em4 = 0.0D0

      end function


      doubleprecision function rrqq2qqht3s7e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.3141592653589793D1 ** 2
      t5 = lh ** 2
      t7 = -0.30D2 * t3 + 0.180D3 * t5
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
      t32 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.10D1)
      t40 = -0.2884936567583026D3 - 0.120D3 * t5 * lh + 0.60D2 * lh * t3
      t41 = 0.3141592653589793D1 * t40
      t46 = t23 * t19 * 0.3141592653589793D1
      t50 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.10D1)
      t54 = t3 ** 2
      t55 = t5 ** 2
      t67 = t23 ** 2
      t72 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.10D1)
      t76 = 0.3141592653589793D1 * t27
      t77 = x1 ** 2
      t78 = t77 * t11
      t79 = t13 * t16
      t80 = t79 * x4
      t83 = log(0.4D1 * t78 * t80)
      t85 = t83 ** 2
      t92 = 0.3141592653589793D1 * lh
      t93 = t27 * t31
      t99 = t93 * t72
      t102 = 0.1D1 / x1
      t104 = 0.1D1 / x4
      t107 = t78 * t79
      t109 = log(0.4D1 * t107)
      t115 = t109 ** 2
      t125 = t41 * t99
      t136 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.1
     #0D1)
      t137 = x3 * t77
      t138 = t137 * t11
      t139 = -0.1D1 + x3
      t140 = t139 * x4
      t144 = log(-0.4D1 * t138 * t79 * t140)
      t145 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.1
     #0D1)
      t149 = log(0.4D1 * t138 * t80)
      t155 = -t72 + t145
      t156 = t93 * t155
      t160 = 0.1D1 / x3
      t162 = t102 * t104
      t165 = t137 * t17
      t167 = log(0.4D1 * t165)
      t169 = t167 ** 2
      t172 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.1
     #0D1)
      t173 = t79 * t139
      t176 = log(-0.4D1 * t138 * t173)
      t178 = t176 ** 2
      t196 = t16 * x4
      t199 = log(0.4D1 * t14 * t196)
      t205 = t199 ** 2
      t225 = x3 * t11
      t226 = t225 * t13
      t227 = t16 * t139
      t231 = log(-0.4D1 * t226 * t227 * x4)
      t233 = t231 ** 2
      t238 = log(0.4D1 * t225 * t80)
      t240 = t238 ** 2
      t254 = -t93 * t155
      t262 = log(-0.4D1 * t225 * t173)
      t264 = t225 * t79
      t266 = log(0.4D1 * t264)
      t272 = t266 ** 2
      t279 = t262 ** 2
      t303 = (t8 + 0.180D3 * t20 * lh + 0.45D2 * t24) * t27 * t31 * t32 
     #/ 0.1440D4 + (t41 - t20 * t7 - 0.90D2 * t24 * lh - 0.15D2 * t46) *
     # t27 * t31 * t50 / 0.1440D4 + (0.3141592653589793D1 * (t54 + 0.60D
     #2 * t55 + 0.5769873135166051D3 * lh - 0.60D2 * t5 * t3) - t20 * t4
     #0 + t24 * t7 / 0.2D1 + 0.30D2 * t46 * lh + 0.15D2 / 0.4D1 * t67 * 
     #0.3141592653589793D1) * t27 * t31 * t72 / 0.1440D4 - (0.90D2 * t76
     # * t31 * (-t32 + t83 * t50 - t85 * t72 / 0.2D1) - 0.180D3 * t92 * 
     #t93 * (-t50 + t83 * t72) - t8 * t99) * t102 * t104 / 0.720D3 - (t8
     # * t93 * (-t50 + t109 * t72) + 0.90D2 * t76 * t31 * (t109 * t32 - 
     #t115 * t50 / 0.2D1 + t115 * t109 * t72 / 0.6D1) - t125 - 0.180D3 *
     # t92 * t93 * (-t32 + t109 * t50 - t115 * t72 / 0.2D1)) * t102 / 0.
     #720D3 - (0.90D2 * t76 * t31 * (t136 - t144 * t145 - t50 + t149 * t
     #72) - 0.180D3 * t92 * t156) * t160 * t162 / 0.720D3 - (0.90D2 * t7
     #6 * t31 * (-t32 + t167 * t50 - t169 * t72 / 0.2D1 + t172 - t176 * 
     #t136 + t178 * t145 / 0.2D1) - 0.180D3 * t92 * t93 * (-t50 + t167 *
     # t72 + t136 - t176 * t145) + t8 * t156) * t160 * t102 / 0.720D3 + 
     #(t8 * t93 * (t50 - t199 * t72) + 0.90D2 * t76 * t31 * (-t199 * t32
     # + t205 * t50 / 0.2D1 - t205 * t199 * t72 / 0.6D1) + t125 - 0.180D
     #3 * t92 * t93 * (t32 - t199 * t50 + t205 * t72 / 0.2D1)) * t104 / 
     #0.1440D4 + (0.90D2 * t76 * t31 * (-t172 + t231 * t136 - t233 * t14
     #5 / 0.2D1 + t32 - t238 * t50 + t240 * t72 / 0.2D1) - 0.180D3 * t92
     # * t93 * (-t136 + t231 * t145 + t50 - t238 * t72) + t8 * t254) * t
     #160 * t104 / 0.1440D4 + (t8 * t93 * (-t136 + t262 * t145 + t50 - t
     #266 * t72) + 0.90D2 * t76 * t31 * (-t266 * t32 + t272 * t50 / 0.2D
     #1 - t272 * t266 * t72 / 0.6D1 + t262 * t172 - t279 * t136 / 0.2D1 
     #+ t279 * t262 * t145 / 0.6D1) + t41 * t254 - 0.180D3 * t92 * t93 *
     # (-t172 + t262 * t136 - t279 * t145 / 0.2D1 + t32 - t266 * t50 + t
     #272 * t72 / 0.2D1)) * t160 / 0.1440D4
      t304 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t303)
      t306 = -0.1D1 + x4
      t309 = -t306
      t310 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #t309)
      t311 = t196 * t306
      t314 = log(-0.4D1 * t14 * t311)
      t315 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #t309)
      t320 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #t309)
      t322 = t314 ** 2
      t332 = t93 * t315
      t346 = log(-0.4D1 * t226 * t311)
      t348 = t346 ** 2
      t351 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, t30
     #9)
      t352 = x4 * t306
      t353 = t227 * t352
      t356 = log(0.4D1 * t226 * t353)
      t357 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, t30
     #9)
      t359 = t356 ** 2
      t360 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, t30
     #9)
      t373 = -t315 + t360
      t383 = log(-0.4D1 * t78 * t13 * t311)
      t385 = t383 ** 2
      t405 = log(0.4D1 * t137 * t14 * t353)
      t410 = log(-0.4D1 * t138 * t79 * t352)
      t424 = (-t8 * t93 * (t310 - t314 * t315) - 0.90D2 * t76 * t31 * (-
     #t314 * t320 + t322 * t310 / 0.2D1 - t322 * t314 * t315 / 0.6D1) - 
     #t41 * t332 + 0.180D3 * t92 * t93 * (t320 - t314 * t310 + t322 * t3
     #15 / 0.2D1)) * t104 / 0.1440D4 + (0.90D2 * t76 * t31 * (-t320 + t3
     #46 * t310 - t348 * t315 / 0.2D1 + t351 - t356 * t357 + t359 * t360
     # / 0.2D1) - 0.180D3 * t92 * t93 * (-t310 + t346 * t315 + t357 - t3
     #56 * t360) + t8 * t93 * t373) * t160 * t104 / 0.1440D4 - (0.90D2 *
     # t76 * t31 * (t320 - t383 * t310 + t385 * t315 / 0.2D1) - 0.180D3 
     #* t92 * t93 * (t310 - t383 * t315) + t8 * t332) * t102 * t104 / 0.
     #720D3 - (0.90D2 * t76 * t31 * (-t357 + t405 * t360 + t310 - t410 *
     # t315) + 0.180D3 * t92 * t93 * t373) * t160 * t162 / 0.720D3
      t425 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t306, t2 * x4, 0.0D0,
     # t424)
      t427 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t428 = s * t427
      t429 = t1 * x1
      t431 = -0.1D1 + x1
      t432 = t1 * t431
      t434 = t427 ** 2
      t440 = 0.1D1 / (-0.2D1 + t427)
      t441 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t442 = t440 * t441
      t443 = t431 ** 2
      t444 = t77 * t443
      t445 = t434 ** 2
      t447 = t444 * x4 * t445
      t450 = log(0.4D1 * t17 * t447)
      t451 = t450 * t440
      t452 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t454 = t450 ** 2
      t456 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t463 = t440 * t452
      t469 = t8 * t27
      t471 = t31 * t440 * t456
      t472 = t469 * t471
      t479 = log(0.4D1 * t17 * t444 * t445)
      t480 = t479 * t440
      t486 = t479 ** 2
      t487 = t486 * t440
      t511 = log(0.4D1 * t264 * t447)
      t518 = t92 * t27
      t529 = log(0.4D1 * t226 * t16 * t77 * t443 * t445)
      t530 = t529 * t440
      t532 = t529 ** 2
      t549 = -(0.90D2 * t76 * t31 * (-t442 + t451 * t452 - t454 * t440 *
     # t456 / 0.2D1) - 0.180D3 * t92 * t93 * (-t463 + t451 * t456) - t47
     #2) * t102 * t104 / 0.720D3 - (-t8 * t93 * (t463 - t480 * t456) - 0
     #.90D2 * t76 * t31 * (-t480 * t441 + t487 * t452 / 0.2D1 - t486 * t
     #479 * t440 * t456 / 0.6D1) - t41 * t27 * t471 + 0.180D3 * t92 * t9
     #3 * (t442 - t480 * t452 + t487 * t456 / 0.2D1)) * t102 / 0.720D3 -
     # (0.90D2 * t76 * t31 * (-t463 + t511 * t440 * t456) + 0.180D3 * t5
     #18 * t471) * t160 * t162 / 0.720D3 - (0.90D2 * t76 * t31 * (-t442 
     #+ t530 * t452 - t532 * t440 * t456 / 0.2D1) - 0.180D3 * t92 * t93 
     #* (-t463 + t530 * t456) - t472) * t160 * t102 / 0.720D3
      t550 = FJET(XB1, XB2, s, 0.0D0, t428 * t429, -t428 * t432, 0.0D0, 
     #-s * t434 * t15 * x1 * t431, t549)
      t552 = KAPPA2(x1, x2, 0.0D0, t309, z)
      t553 = s * t552
      t555 = t432 * t306
      t557 = t432 * x4
      t559 = t552 ** 2
      t562 = x1 * t431
      t566 = 0.1D1 / (-0.2D1 + t552)
      t567 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t30
     #9)
      t570 = t559 ** 2
      t575 = log(-0.4D1 * t107 * t443 * x4 * t306 * t570)
      t576 = t575 * t566
      t577 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t30
     #9)
      t579 = t575 ** 2
      t581 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t30
     #9)
      t588 = t566 * t577
      t595 = t31 * t566 * t581
      t604 = log(-0.4D1 * t264 * t444 * t352 * t570)
      t617 = -(0.90D2 * t76 * t31 * (t566 * t567 - t576 * t577 + t579 * 
     #t566 * t581 / 0.2D1) - 0.180D3 * t92 * t93 * (t588 - t576 * t581) 
     #+ t469 * t595) * t102 * t104 / 0.720D3 - (0.90D2 * t76 * t31 * (t5
     #88 - t604 * t566 * t581) - 0.180D3 * t518 * t595) * t160 * t162 / 
     #0.720D3
      t618 = FJET(XB1, XB2, s, 0.0D0, t553 * t429, t553 * t555, -t553 * 
     #t557, s * t559 * t15 * t562 * t306, t617)
      t620 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t621 = s * t620
      t622 = t429 * x3
      t624 = t429 * t139
      t627 = t620 ** 2
      t633 = 0.1D1 / (-0.2D1 + t620)
      t634 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t635 = t633 * t634
      t636 = t627 ** 2
      t641 = log(-0.4D1 * t264 * t444 * t140 * t636)
      t643 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t650 = t31 * t633 * t643
      t656 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t662 = log(-0.4D1 * t264 * t444 * t139 * t636)
      t663 = t662 * t633
      t665 = t662 ** 2
      t683 = -(0.90D2 * t76 * t31 * (t635 - t641 * t633 * t643) - 0.180D
     #3 * t518 * t650) * t160 * t162 / 0.720D3 - (0.90D2 * t76 * t31 * (
     #t633 * t656 - t663 * t634 + t665 * t633 * t643 / 0.2D1) - 0.180D3 
     #* t92 * t93 * (t635 - t663 * t643) + t469 * t650) * t160 * t102 / 
     #0.720D3
      t684 = FJET(XB1, XB2, s, t621 * t622, -t621 * t624, -t621 * t432, 
     #0.0D0, s * t627 * t15 * t562 * t139, t683)
      t686 = KAPPA2(x1, x2, x3, t309, z)
      t687 = s * t686
      t692 = t686 ** 2
      t697 = cos(t9)
      t700 = Sqrt(x3 * t139 * t352)
      t707 = 0.1D1 / (-0.2D1 + t686)
      t708 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, t309)
      t711 = t692 ** 2
      t716 = log(0.4D1 * t165 * t443 * t139 * t352 * t711)
      t718 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, t309)
      t728 = -0.90D2 * t76 * t31 * (t707 * t708 - t716 * t707 * t718) + 
     #0.180D3 * t518 * t31 * t707 * t718
      t732 = FJET(XB1, XB2, s, t687 * t622, -t687 * t624, t687 * t555, -
     #t687 * t557, s * t692 * t15 * t562 * (-0.1D1 + x3 + x4 - 0.2D1 * x
     #3 * x4 + 0.2D1 * t697 * t700), -t728 * t160 * t162 / 0.720D3)
      rrqq2qqht3s7e1 = t304 * t303 + t425 * t424 + t550 * t549 + t618 * 
     #t617 + t684 * t683 - t732 * t728 * t160 * t102 * t104 / 0.720D3

      end function



      doubleprecision function rrqq2qqht3s7e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.3141592653589793D1 * lh
      t5 = x2 * 0.3141592653589793D1
      t6 = sin(t5)
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
      t24 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.10D1)
      t28 = 0.3141592653589793D1 ** 2
      t30 = lh ** 2
      t32 = -0.30D2 * t28 + 0.180D3 * t30
      t33 = 0.3141592653589793D1 * t32
      t36 = t15 ** 2
      t37 = t36 * 0.3141592653589793D1
      t41 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.10D1)
      t59 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.10D1)
      t63 = t19 * 0.3141592653589793D1
      t64 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.10
     #D1)
      t65 = x3 * t7
      t66 = t65 * t9
      t67 = -0.1D1 + x3
      t68 = t12 * t67
      t72 = log(-0.4D1 * t66 * t68 * x4)
      t73 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.10
     #D1)
      t75 = t9 * t12
      t76 = t75 * x4
      t79 = log(0.4D1 * t65 * t76)
      t85 = t19 * t23
      t86 = -t73 + t59
      t87 = t85 * t86
      t91 = 0.1D1 / x3
      t93 = 0.1D1 / x4
      t96 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.10
     #D1)
      t97 = t75 * t67
      t100 = log(-0.4D1 * t65 * t97)
      t102 = t100 ** 2
      t105 = t65 * t75
      t107 = log(0.4D1 * t105)
      t109 = t107 ** 2
      t126 = t12 * x4
      t129 = log(0.4D1 * t10 * t126)
      t131 = t129 ** 2
      t143 = t85 * t59
      t144 = t33 * t143
      t148 = x1 ** 2
      t149 = t148 * t7
      t152 = log(0.4D1 * t149 * t76)
      t161 = 0.1D1 / x1
      t165 = t63 * t23
      t166 = -t86
      t168 = t161 * t93
      t172 = x3 * t148
      t175 = log(0.4D1 * t172 * t13)
      t180 = log(-0.4D1 * t172 * t7 * t97)
      t193 = t149 * t75
      t195 = log(0.4D1 * t193)
      t197 = t195 ** 2
      t212 = (-0.180D3 * t3 - 0.90D2 * t16) * t19 * t23 * t24 / 0.1440D4
     # + (t33 + 0.180D3 * t16 * lh + 0.45D2 * t37) * t19 * t23 * t41 / 0
     #.1440D4 + (0.3141592653589793D1 * (-0.2884936567583026D3 - 0.120D3
     # * t30 * lh + 0.60D2 * lh * t28) - t16 * t32 - 0.90D2 * t37 * lh -
     # 0.15D2 * t36 * t15 * 0.3141592653589793D1) * t19 * t23 * t59 / 0.
     #1440D4 + (0.90D2 * t63 * t23 * (-t64 + t72 * t73 + t41 - t79 * t59
     #) - 0.180D3 * t3 * t87) * t91 * t93 / 0.1440D4 + (0.90D2 * t63 * t
     #23 * (-t96 + t100 * t64 - t102 * t73 / 0.2D1 + t24 - t107 * t41 + 
     #t109 * t59 / 0.2D1) - 0.180D3 * t3 * t85 * (-t64 + t100 * t73 + t4
     #1 - t107 * t59) + t33 * t87) * t91 / 0.1440D4 + (0.90D2 * t63 * t2
     #3 * (t24 - t129 * t41 + t131 * t59 / 0.2D1) - 0.180D3 * t3 * t85 *
     # (t41 - t129 * t59) + t144) * t93 / 0.1440D4 - (0.90D2 * t63 * t23
     # * (-t41 + t152 * t59) + 0.180D3 * t3 * t143) * t161 * t93 / 0.720
     #D3 - t165 * t166 * t91 * t168 / 0.8D1 - (0.90D2 * t63 * t23 * (-t4
     #1 + t175 * t59 + t64 - t180 * t73) - 0.180D3 * t3 * t85 * t166) * 
     #t91 * t161 / 0.720D3 - (0.90D2 * t63 * t23 * (-t24 + t195 * t41 - 
     #t197 * t59 / 0.2D1) - 0.180D3 * t3 * t85 * (-t41 + t195 * t59) - t
     #144) * t161 / 0.720D3
      t213 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t212)
      t215 = -0.1D1 + x4
      t218 = -t215
      t219 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #t218)
      t220 = t126 * t215
      t223 = log(-0.4D1 * t10 * t220)
      t224 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #t218)
      t226 = t223 ** 2
      t227 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #t218)
      t239 = t85 * t227
      t247 = log(-0.4D1 * t149 * t9 * t220)
      t259 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, t21
     #8)
      t260 = t227 - t259
      t267 = log(-0.4D1 * t66 * t220)
      t269 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, t21
     #8)
      t270 = x4 * t215
      t274 = log(0.4D1 * t66 * t68 * t270)
      t288 = (-0.90D2 * t63 * t23 * (t219 - t223 * t224 + t226 * t227 / 
     #0.2D1) + 0.180D3 * t3 * t85 * (t224 - t223 * t227) - t33 * t239) *
     # t93 / 0.1440D4 - (0.90D2 * t63 * t23 * (t224 - t247 * t227) - 0.1
     #80D3 * t3 * t239) * t161 * t93 / 0.720D3 - t165 * t260 * t91 * t16
     #8 / 0.8D1 + (0.90D2 * t63 * t23 * (-t224 + t267 * t227 + t269 - t2
     #74 * t259) + 0.180D3 * t3 * t85 * t260) * t91 * t93 / 0.1440D4
      t289 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t215, t2 * x4, 0.0D0,
     # t288)
      t291 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t292 = s * t291
      t293 = t1 * x1
      t295 = -0.1D1 + x1
      t296 = t1 * t295
      t298 = t291 ** 2
      t304 = 0.1D1 / (-0.2D1 + t291)
      t305 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t306 = t304 * t305
      t307 = t295 ** 2
      t308 = t148 * t307
      t309 = t298 ** 2
      t314 = log(0.4D1 * t13 * t308 * x4 * t309)
      t316 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t322 = t3 * t19
      t323 = t23 * t304
      t324 = t323 * t316
      t326 = 0.180D3 * t322 * t324
      t341 = log(0.4D1 * t66 * t12 * t148 * t307 * t309)
      t352 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t357 = log(0.4D1 * t13 * t308 * t309)
      t358 = t357 * t304
      t360 = t357 ** 2
      t378 = -(0.90D2 * t63 * t23 * (-t306 + t314 * t304 * t316) + t326)
     # * t161 * t93 / 0.720D3 + t63 * t323 * t316 * t91 * t168 / 0.8D1 -
     # (0.90D2 * t63 * t23 * (-t306 + t341 * t304 * t316) + t326) * t91 
     #* t161 / 0.720D3 - (-0.90D2 * t63 * t23 * (t304 * t352 - t358 * t3
     #05 + t360 * t304 * t316 / 0.2D1) + 0.180D3 * t3 * t85 * (t306 - t3
     #58 * t316) - t33 * t19 * t324) * t161 / 0.720D3
      t379 = FJET(XB1, XB2, s, 0.0D0, t292 * t293, -t292 * t296, 0.0D0, 
     #-s * t298 * t11 * x1 * t295, t378)
      t381 = KAPPA2(x1, x2, 0.0D0, t218, z)
      t382 = s * t381
      t384 = t296 * t215
      t386 = t296 * x4
      t388 = t381 ** 2
      t391 = x1 * t295
      t395 = 0.1D1 / (-0.2D1 + t381)
      t396 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t21
     #8)
      t399 = t388 ** 2
      t404 = log(-0.4D1 * t193 * t307 * x4 * t215 * t399)
      t406 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t21
     #8)
      t412 = t23 * t395
      t425 = -(0.90D2 * t63 * t23 * (t395 * t396 - t404 * t395 * t406) -
     # 0.180D3 * t322 * t412 * t406) * t161 * t93 / 0.720D3 - t63 * t412
     # * t406 * t91 * t168 / 0.8D1
      t426 = FJET(XB1, XB2, s, 0.0D0, t382 * t293, t382 * t384, -t382 * 
     #t386, s * t388 * t11 * t391 * t215, t425)
      t428 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t429 = s * t428
      t430 = t293 * x3
      t432 = t293 * t67
      t435 = t428 ** 2
      t441 = 0.1D1 / (-0.2D1 + t428)
      t442 = t23 * t441
      t444 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t449 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t451 = t435 ** 2
      t456 = log(-0.4D1 * t105 * t308 * t67 * t451)
      t470 = -t63 * t442 * t444 * t91 * t168 / 0.8D1 - (0.90D2 * t63 * t
     #23 * (t441 * t449 - t456 * t441 * t444) - 0.180D3 * t322 * t442 * 
     #t444) * t91 * t161 / 0.720D3
      t471 = FJET(XB1, XB2, s, t429 * t430, -t429 * t432, -t429 * t296, 
     #0.0D0, s * t435 * t11 * t391 * t67, t470)
      t473 = KAPPA2(x1, x2, x3, t218, z)
      t474 = s * t473
      t479 = t473 ** 2
      t484 = cos(t5)
      t487 = Sqrt(x3 * t67 * t270)
      t494 = 0.1D1 / (-0.2D1 + t473)
      t497 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, t218)
      t502 = FJET(XB1, XB2, s, t474 * t430, -t474 * t432, t474 * t384, -
     #t474 * t386, s * t479 * t11 * t391 * (-0.1D1 + x3 + x4 - 0.2D1 * x
     #3 * x4 + 0.2D1 * t484 * t487), t63 * t23 * t494 * t497 * t91 * t16
     #8 / 0.8D1)
      rrqq2qqht3s7e0 = t213 * t212 + t289 * t288 + t379 * t378 + t426 * 
     #t425 + t471 * t470 + t502 * 0.3141592653589793D1 * t85 * t494 * t4
     #97 * t91 * t161 * t93 / 0.8D1

      end function



      doubleprecision function rrqq2qqht3s7em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0.
     #10D1)
      t12 = 0.3141592653589793D1 * lh
      t15 = sin(x2 * 0.3141592653589793D1)
      t16 = t15 ** 2
      t17 = z ** 2
      t18 = 0.1D1 / t17
      t19 = t16 * t18
      t20 = t1 ** 2
      t21 = t20 ** 2
      t22 = t19 * t21
      t24 = log(0.4D1 * t22)
      t25 = t24 * 0.3141592653589793D1
      t29 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.10D1)
      t33 = 0.3141592653589793D1 ** 2
      t35 = lh ** 2
      t41 = t24 ** 2
      t46 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.10D1)
      t50 = t4 * t7
      t51 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.10
     #D1)
      t52 = -t51 + t46
      t53 = 0.1D1 / x3
      t55 = 0.1D1 / x4
      t59 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.10
     #D1)
      t60 = x3 * t16
      t61 = t18 * t21
      t62 = -0.1D1 + x3
      t66 = log(-0.4D1 * t60 * t61 * t62)
      t70 = log(0.4D1 * t60 * t61)
      t76 = t3 * t7
      t83 = t21 * x4
      t86 = log(0.4D1 * t19 * t83)
      t94 = 0.180D3 * t12 * t76 * t46
      t100 = 0.1D1 / x1
      t104 = x1 ** 2
      t108 = log(0.4D1 * t104 * t16 * t61)
      t121 = t4 * t7 * t8 / 0.16D2 + (-0.180D3 * t12 - 0.90D2 * t25) * t
     #3 * t7 * t29 / 0.1440D4 + (0.3141592653589793D1 * (-0.30D2 * t33 +
     # 0.180D3 * t35) + 0.180D3 * t25 * lh + 0.45D2 * t41 * 0.3141592653
     #589793D1) * t3 * t7 * t46 / 0.1440D4 + t50 * t52 * t53 * t55 / 0.1
     #6D2 + (0.90D2 * t4 * t7 * (-t59 + t66 * t51 + t29 - t70 * t46) - 0
     #.180D3 * t12 * t76 * t52) * t53 / 0.1440D4 + (0.90D2 * t4 * t7 * (
     #t29 - t86 * t46) - t94) * t55 / 0.1440D4 + t50 * t52 * t53 * t100 
     #/ 0.8D1 - (0.90D2 * t4 * t7 * (-t29 + t108 * t46) + t94) * t100 / 
     #0.720D3 + t50 * t46 * t100 * t55 / 0.8D1
      t122 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t121)
      t124 = -0.1D1 + x4
      t127 = -t124
      t128 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #t127)
      t132 = log(-0.4D1 * t19 * t83 * t124)
      t133 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #t127)
      t149 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, t12
     #7)
      t155 = (-0.90D2 * t4 * t7 * (t128 - t132 * t133) + 0.180D3 * t12 *
     # t76 * t133) * t55 / 0.1440D4 - t50 * t133 * t100 * t55 / 0.8D1 + 
     #t50 * (-t133 + t149) * t53 * t55 / 0.16D2
      t156 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t124, t2 * x4, 0.0D0,
     # t155)
      t158 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t159 = s * t158
      t160 = t1 * x1
      t162 = -0.1D1 + x1
      t163 = t1 * t162
      t165 = t158 ** 2
      t171 = 0.1D1 / (-0.2D1 + t158)
      t172 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t173 = t171 * t172
      t174 = t53 * t100
      t178 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.1
     #0D1)
      t180 = t162 ** 2
      t182 = t165 ** 2
      t186 = log(0.4D1 * t22 * t104 * t180 * t182)
      t201 = t100 * t55
      t205 = t50 * t173 * t174 / 0.8D1 - (-0.90D2 * t4 * t7 * (t171 * t1
     #78 - t186 * t171 * t172) + 0.180D3 * t12 * t3 * t7 * t171 * t172) 
     #* t100 / 0.720D3 + t50 * t173 * t201 / 0.8D1
      t206 = FJET(XB1, XB2, s, 0.0D0, t159 * t160, -t159 * t163, 0.0D0, 
     #-s * t165 * t20 * x1 * t162, t205)
      t208 = KAPPA2(x1, x2, 0.0D0, t127, z)
      t209 = s * t208
      t215 = t208 ** 2
      t218 = x1 * t162
      t223 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, t12
     #7)
      t225 = 0.1D1 / (-0.2D1 + t208) * t223 * t201
      t228 = FJET(XB1, XB2, s, 0.0D0, t209 * t160, t209 * t163 * t124, -
     #t209 * t163 * x4, s * t215 * t20 * t218 * t124, -t50 * t225 / 0.8D
     #1)
      t233 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t234 = s * t233
      t240 = t233 ** 2
      t247 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.10D1
     #)
      t249 = 0.1D1 / (-0.2D1 + t233) * t247 * t174
      t252 = FJET(XB1, XB2, s, t234 * t160 * x3, -t234 * t160 * t62, -t2
     #34 * t163, 0.0D0, s * t240 * t20 * t218 * t62, -t50 * t249 / 0.8D1
     #)
      rrqq2qqht3s7em1 = t122 * t121 + t156 * t155 + t206 * t205 - t228 *
     # 0.3141592653589793D1 * t76 * t225 / 0.8D1 - t252 * 0.314159265358
     #9793D1 * t76 * t249 / 0.8D1

      end function



      doubleprecision function rrqq2qqht3s7em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0.
     #10D1)
      t9 = t7 * t8
      t10 = 0.1D1 / x4
      t14 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.10
     #D1)
      t21 = 0.1D1 / x1
      t25 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.10D1)
      t32 = sin(x2 * 0.3141592653589793D1)
      t33 = t32 ** 2
      t34 = z ** 2
      t37 = t1 ** 2
      t38 = t37 ** 2
      t41 = log(0.4D1 * t33 / t34 * t38)
      t48 = t4 * t9 * t10 / 0.16D2 + t4 * t7 * (-t14 + t8) / x3 / 0.16D2
     # + t4 * t9 * t21 / 0.8D1 + t4 * t7 * t25 / 0.16D2 + (-0.180D3 * 0.
     #3141592653589793D1 * lh - 0.90D2 * t41 * 0.3141592653589793D1) * t
     #3 * t9 / 0.1440D4
      t49 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t48)
      t51 = -0.1D1 + x4
      t55 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, -
     #t51)
      t57 = t7 * t55 * t10
      t60 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t2 * t51, t2 * x4, 0.0D0, -
     #t4 * t57 / 0.16D2)
      t65 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t66 = s * t65
      t69 = -0.1D1 + x1
      t72 = t65 ** 2
      t79 = 0.1D1 / (-0.2D1 + t65)
      t80 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.10
     #D1)
      t85 = FJET(XB1, XB2, s, 0.0D0, t66 * t1 * x1, -t66 * t1 * t69, 0.0
     #D0, -s * t72 * t37 * x1 * t69, t4 * t7 * t79 * t80 * t21 / 0.8D1)
      rrqq2qqht3s7em2 = t49 * t48 - t60 * 0.3141592653589793D1 * t3 * t5
     #7 / 0.16D2 + t85 * 0.3141592653589793D1 * t3 * t7 * t79 * t80 * t2
     #1 / 0.8D1

      end function



      doubleprecision function rrqq2qqht3s7em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      t1 = z - 0.1D1
      t3 = 0.1D1 / t1
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0.
     #10D1)
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, s * t1, 0.0D0, 0.0D0, 0.3141
     #592653589793D1 * t3 * t7 * t8 / 0.16D2)
      rrqq2qqht3s7em3 = t12 * 0.3141592653589793D1 * t3 * t7 * t8 / 0.16
     #D2

      end function



      doubleprecision function rrqq2qqht3s7em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      rrqq2qqht3s7em4 = 0.0D0

      end function


      doubleprecision function rrqq2qqht3s8e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.3141592653589793D1 ** 2
      t5 = lh ** 2
      t7 = -0.30D2 * t3 + 0.180D3 * t5
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
      t32 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t40 = -0.2884936567583026D3 - 0.120D3 * t5 * lh + 0.60D2 * lh * t3
      t41 = 0.3141592653589793D1 * t40
      t46 = t23 * t19 * 0.3141592653589793D1
      t50 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t54 = t3 ** 2
      t55 = t5 ** 2
      t67 = t23 ** 2
      t72 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t76 = 0.3141592653589793D1 * t27
      t77 = x1 ** 2
      t78 = t77 * t11
      t79 = t13 * t16
      t80 = t79 * x4
      t83 = log(0.4D1 * t78 * t80)
      t85 = t83 ** 2
      t92 = 0.3141592653589793D1 * lh
      t93 = t27 * t31
      t99 = t93 * t72
      t102 = 0.1D1 / x1
      t104 = 0.1D1 / x4
      t107 = t78 * t79
      t109 = log(0.4D1 * t107)
      t115 = t109 ** 2
      t125 = t41 * t99
      t136 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.0
     #D0)
      t137 = x3 * t77
      t138 = t137 * t11
      t139 = -0.1D1 + x3
      t140 = t139 * x4
      t144 = log(-0.4D1 * t138 * t79 * t140)
      t145 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.0
     #D0)
      t149 = log(0.4D1 * t138 * t80)
      t155 = -t72 + t145
      t156 = t93 * t155
      t160 = 0.1D1 / x3
      t162 = t102 * t104
      t165 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.0
     #D0)
      t166 = t79 * t139
      t169 = log(-0.4D1 * t138 * t166)
      t171 = t169 ** 2
      t174 = t137 * t17
      t176 = log(0.4D1 * t174)
      t178 = t176 ** 2
      t196 = t16 * x4
      t199 = log(0.4D1 * t14 * t196)
      t205 = t199 ** 2
      t225 = x3 * t11
      t226 = t225 * t13
      t227 = t16 * t139
      t231 = log(-0.4D1 * t226 * t227 * x4)
      t233 = t231 ** 2
      t238 = log(0.4D1 * t225 * t80)
      t240 = t238 ** 2
      t254 = -t93 * t155
      t262 = log(0.4D1 * t225 * t79)
      t266 = log(-0.4D1 * t225 * t166)
      t272 = t266 ** 2
      t279 = t262 ** 2
      t303 = (t8 + 0.180D3 * t20 * lh + 0.45D2 * t24) * t27 * t31 * t32 
     #/ 0.1440D4 + (t41 - t20 * t7 - 0.90D2 * t24 * lh - 0.15D2 * t46) *
     # t27 * t31 * t50 / 0.1440D4 + (0.3141592653589793D1 * (t54 + 0.60D
     #2 * t55 + 0.5769873135166051D3 * lh - 0.60D2 * t5 * t3) - t20 * t4
     #0 + t24 * t7 / 0.2D1 + 0.30D2 * t46 * lh + 0.15D2 / 0.4D1 * t67 * 
     #0.3141592653589793D1) * t27 * t31 * t72 / 0.1440D4 - (0.90D2 * t76
     # * t31 * (-t32 + t83 * t50 - t85 * t72 / 0.2D1) - 0.180D3 * t92 * 
     #t93 * (-t50 + t83 * t72) - t8 * t99) * t102 * t104 / 0.720D3 + (t8
     # * t93 * (t50 - t109 * t72) + 0.90D2 * t76 * t31 * (-t109 * t32 + 
     #t115 * t50 / 0.2D1 - t115 * t109 * t72 / 0.6D1) + t125 - 0.180D3 *
     # t92 * t93 * (t32 - t109 * t50 + t115 * t72 / 0.2D1)) * t102 / 0.7
     #20D3 - (0.90D2 * t76 * t31 * (t136 - t144 * t145 - t50 + t149 * t7
     #2) - 0.180D3 * t92 * t156) * t160 * t162 / 0.720D3 - (0.90D2 * t76
     # * t31 * (t165 - t169 * t136 + t171 * t145 / 0.2D1 - t32 + t176 * 
     #t50 - t178 * t72 / 0.2D1) - 0.180D3 * t92 * t93 * (t136 - t169 * t
     #145 - t50 + t176 * t72) + t8 * t156) * t160 * t102 / 0.720D3 + (t8
     # * t93 * (t50 - t199 * t72) + 0.90D2 * t76 * t31 * (-t199 * t32 + 
     #t205 * t50 / 0.2D1 - t205 * t199 * t72 / 0.6D1) + t125 - 0.180D3 *
     # t92 * t93 * (t32 - t199 * t50 + t205 * t72 / 0.2D1)) * t104 / 0.1
     #440D4 + (0.90D2 * t76 * t31 * (-t165 + t231 * t136 - t233 * t145 /
     # 0.2D1 + t32 - t238 * t50 + t240 * t72 / 0.2D1) - 0.180D3 * t92 * 
     #t93 * (-t136 + t231 * t145 + t50 - t238 * t72) + t8 * t254) * t160
     # * t104 / 0.1440D4 + (t8 * t93 * (t50 - t262 * t72 - t136 + t266 *
     # t145) + 0.90D2 * t76 * t31 * (t266 * t165 - t272 * t136 / 0.2D1 +
     # t272 * t266 * t145 / 0.6D1 - t262 * t32 + t279 * t50 / 0.2D1 - t2
     #79 * t262 * t72 / 0.6D1) + t41 * t254 - 0.180D3 * t92 * t93 * (t32
     # - t262 * t50 + t279 * t72 / 0.2D1 - t165 + t266 * t136 - t272 * t
     #145 / 0.2D1)) * t160 / 0.1440D4
      t304 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t303)
      t307 = -0.1D1 + x4
      t309 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t310 = t196 * t307
      t313 = log(-0.4D1 * t14 * t310)
      t314 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t319 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t321 = t313 ** 2
      t331 = t93 * t314
      t343 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t344 = x4 * t307
      t345 = t227 * t344
      t348 = log(0.4D1 * t226 * t345)
      t349 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t351 = t348 ** 2
      t352 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t357 = log(-0.4D1 * t226 * t310)
      t359 = t357 ** 2
      t372 = -t314 + t352
      t379 = t78 * t13
      t382 = log(-0.4D1 * t379 * t310)
      t384 = t382 ** 2
      t404 = log(-0.4D1 * t138 * t79 * t344)
      t406 = t137 * t14
      t409 = log(0.4D1 * t406 * t345)
      t423 = (-t8 * t93 * (t309 - t313 * t314) - 0.90D2 * t76 * t31 * (-
     #t313 * t319 + t321 * t309 / 0.2D1 - t321 * t313 * t314 / 0.6D1) - 
     #t41 * t331 + 0.180D3 * t92 * t93 * (t319 - t313 * t309 + t321 * t3
     #14 / 0.2D1)) * t104 / 0.1440D4 + (0.90D2 * t76 * t31 * (t343 - t34
     #8 * t349 + t351 * t352 / 0.2D1 - t319 + t357 * t309 - t359 * t314 
     #/ 0.2D1) - 0.180D3 * t92 * t93 * (t349 - t348 * t352 - t309 + t357
     # * t314) + t8 * t93 * t372) * t160 * t104 / 0.1440D4 - (0.90D2 * t
     #76 * t31 * (t319 - t382 * t309 + t384 * t314 / 0.2D1) - 0.180D3 * 
     #t92 * t93 * (t309 - t382 * t314) + t8 * t331) * t102 * t104 / 0.72
     #0D3 - (0.90D2 * t76 * t31 * (t309 - t404 * t314 - t349 + t409 * t3
     #52) + 0.180D3 * t92 * t93 * t372) * t160 * t162 / 0.720D3
      t424 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * t307, 0.0D0,
     # t423)
      t427 = -0.1D1 + x1
      t429 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0
     #D0)
      t430 = t427 ** 2
      t431 = t16 * t430
      t435 = log(0.4D1 * t379 * t431 * x4)
      t436 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0
     #D0)
      t438 = t435 ** 2
      t439 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0
     #D0)
      t451 = t93 * t439
      t452 = t8 * t451
      t456 = t79 * t430
      t459 = log(0.4D1 * t78 * t456)
      t465 = t459 ** 2
      t485 = t430 * x4
      t489 = log(0.4D1 * t138 * t79 * t485)
      t502 = log(0.4D1 * t138 * t456)
      t504 = t502 ** 2
      t520 = -(0.90D2 * t76 * t31 * (t429 - t435 * t436 + t438 * t439 / 
     #0.2D1) - 0.180D3 * t92 * t93 * (t436 - t435 * t439) + t452) * t102
     # * t104 / 0.720D3 + (-t8 * t93 * (t436 - t459 * t439) - 0.90D2 * t
     #76 * t31 * (-t459 * t429 + t465 * t436 / 0.2D1 - t465 * t459 * t43
     #9 / 0.6D1) - t41 * t451 + 0.180D3 * t92 * t93 * (t429 - t459 * t43
     #6 + t465 * t439 / 0.2D1)) * t102 / 0.720D3 - (0.90D2 * t76 * t31 *
     # (t436 - t489 * t439) - 0.180D3 * t92 * t451) * t160 * t162 / 0.72
     #0D3 - (0.90D2 * t76 * t31 * (t429 - t502 * t436 + t504 * t439 / 0.
     #2D1) - 0.180D3 * t92 * t93 * (t436 - t502 * t439) + t452) * t160 *
     # t102 / 0.720D3
      t521 = FJET(XB1, XB2, s, 0.0D0, t2 * x1, 0.0D0, -t2 * t427, 0.0D0,
     # t520)
      t523 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t524 = s * t523
      t525 = t1 * x1
      t527 = t1 * t427
      t528 = t527 * x4
      t530 = t527 * t307
      t532 = t523 ** 2
      t535 = x1 * t427
      t539 = 0.1D1 / (-0.2D1 + t523)
      t540 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t542 = t532 ** 2
      t547 = log(-0.4D1 * t107 * t485 * t307 * t542)
      t548 = t547 * t539
      t549 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t551 = t547 ** 2
      t553 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t560 = t539 * t549
      t566 = t8 * t27
      t568 = t31 * t539 * t553
      t577 = log(-0.4D1 * t406 * t431 * t344 * t542)
      t584 = t92 * t27
      t591 = -(0.90D2 * t76 * t31 * (t539 * t540 - t548 * t549 + t551 * 
     #t539 * t553 / 0.2D1) - 0.180D3 * t92 * t93 * (t560 - t548 * t553) 
     #+ t566 * t568) * t102 * t104 / 0.720D3 - (0.90D2 * t76 * t31 * (t5
     #60 - t577 * t539 * t553) - 0.180D3 * t584 * t568) * t160 * t162 / 
     #0.720D3
      t592 = FJET(XB1, XB2, s, 0.0D0, t524 * t525, -t524 * t528, t524 * 
     #t530, -s * t532 * t15 * t535 * x4, t591)
      t594 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t595 = s * t594
      t596 = t525 * x3
      t598 = t525 * t139
      t601 = t594 ** 2
      t607 = 0.1D1 / (-0.2D1 + t594)
      t608 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0D0)
      t609 = t607 * t608
      t610 = t601 ** 2
      t615 = log(-0.4D1 * t406 * t431 * t140 * t610)
      t617 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0D0)
      t624 = t31 * t607 * t617
      t630 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0D0)
      t636 = log(-0.4D1 * t406 * t431 * t139 * t610)
      t637 = t636 * t607
      t639 = t636 ** 2
      t657 = -(0.90D2 * t76 * t31 * (t609 - t615 * t607 * t617) - 0.180D
     #3 * t584 * t624) * t160 * t162 / 0.720D3 - (0.90D2 * t76 * t31 * (
     #t607 * t630 - t637 * t608 + t639 * t607 * t617 / 0.2D1) - 0.180D3 
     #* t92 * t93 * (t609 - t637 * t617) + t566 * t624) * t160 * t102 / 
     #0.720D3
      t658 = FJET(XB1, XB2, s, t595 * t596, -t595 * t598, 0.0D0, -t595 *
     # t527, -s * t601 * t15 * t535 * x3, t657)
      t660 = KAPPA2(x1, x2, x3, x4, z)
      t661 = s * t660
      t666 = t660 ** 2
      t671 = cos(t9)
      t674 = Sqrt(x3 * t139 * t344)
      t681 = 0.1D1 / (-0.2D1 + t660)
      t682 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t685 = t666 ** 2
      t690 = log(0.4D1 * t174 * t430 * t139 * t344 * t685)
      t692 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t702 = -0.90D2 * t76 * t31 * (t681 * t682 - t690 * t681 * t692) + 
     #0.180D3 * t584 * t31 * t681 * t692
      t706 = FJET(XB1, XB2, s, t661 * t596, -t661 * t598, -t661 * t528, 
     #t661 * t530, s * t666 * t15 * t535 * (-x3 - x4 + 0.2D1 * x3 * x4 +
     # 0.2D1 * t671 * t674), -t702 * t160 * t162 / 0.720D3)
      rrqq2qqht3s8e1 = t304 * t303 + t424 * t423 + t521 * t520 + t592 * 
     #t591 + t658 * t657 - t706 * t702 * t160 * t102 * t104 / 0.720D3

      end function



      doubleprecision function rrqq2qqht3s8e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.3141592653589793D1 * lh
      t5 = x2 * 0.3141592653589793D1
      t6 = sin(t5)
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
      t24 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t28 = 0.3141592653589793D1 ** 2
      t30 = lh ** 2
      t32 = -0.30D2 * t28 + 0.180D3 * t30
      t33 = 0.3141592653589793D1 * t32
      t36 = t15 ** 2
      t37 = t36 * 0.3141592653589793D1
      t41 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t59 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t63 = t19 * 0.3141592653589793D1
      t64 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.0D
     #0)
      t65 = x3 * t7
      t66 = t65 * t9
      t67 = -0.1D1 + x3
      t68 = t12 * t67
      t72 = log(-0.4D1 * t66 * t68 * x4)
      t73 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.0D
     #0)
      t75 = t9 * t12
      t76 = t75 * x4
      t79 = log(0.4D1 * t65 * t76)
      t85 = t19 * t23
      t86 = -t73 + t59
      t87 = t85 * t86
      t91 = 0.1D1 / x3
      t93 = 0.1D1 / x4
      t98 = log(0.4D1 * t65 * t75)
      t100 = t98 ** 2
      t103 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.0
     #D0)
      t104 = t75 * t67
      t107 = log(-0.4D1 * t65 * t104)
      t109 = t107 ** 2
      t126 = t12 * x4
      t129 = log(0.4D1 * t10 * t126)
      t131 = t129 ** 2
      t143 = t85 * t59
      t144 = t33 * t143
      t148 = x1 ** 2
      t149 = t148 * t7
      t152 = log(0.4D1 * t149 * t76)
      t161 = 0.1D1 / x1
      t165 = t63 * t23
      t166 = -t86
      t168 = t161 * t93
      t172 = x3 * t148
      t173 = t172 * t7
      t176 = log(-0.4D1 * t173 * t104)
      t180 = log(0.4D1 * t172 * t13)
      t193 = t149 * t75
      t195 = log(0.4D1 * t193)
      t197 = t195 ** 2
      t212 = (-0.180D3 * t3 - 0.90D2 * t16) * t19 * t23 * t24 / 0.1440D4
     # + (t33 + 0.180D3 * t16 * lh + 0.45D2 * t37) * t19 * t23 * t41 / 0
     #.1440D4 + (0.3141592653589793D1 * (-0.2884936567583026D3 - 0.120D3
     # * t30 * lh + 0.60D2 * lh * t28) - t16 * t32 - 0.90D2 * t37 * lh -
     # 0.15D2 * t36 * t15 * 0.3141592653589793D1) * t19 * t23 * t59 / 0.
     #1440D4 + (0.90D2 * t63 * t23 * (-t64 + t72 * t73 + t41 - t79 * t59
     #) - 0.180D3 * t3 * t87) * t91 * t93 / 0.1440D4 + (0.90D2 * t63 * t
     #23 * (t24 - t98 * t41 + t100 * t59 / 0.2D1 - t103 + t107 * t64 - t
     #109 * t73 / 0.2D1) - 0.180D3 * t3 * t85 * (t41 - t98 * t59 - t64 +
     # t107 * t73) + t33 * t87) * t91 / 0.1440D4 + (0.90D2 * t63 * t23 *
     # (t24 - t129 * t41 + t131 * t59 / 0.2D1) - 0.180D3 * t3 * t85 * (t
     #41 - t129 * t59) + t144) * t93 / 0.1440D4 - (0.90D2 * t63 * t23 * 
     #(-t41 + t152 * t59) + 0.180D3 * t3 * t143) * t161 * t93 / 0.720D3 
     #- t165 * t166 * t91 * t168 / 0.8D1 - (0.90D2 * t63 * t23 * (t64 - 
     #t176 * t73 - t41 + t180 * t59) - 0.180D3 * t3 * t85 * t166) * t91 
     #* t161 / 0.720D3 + (0.90D2 * t63 * t23 * (t24 - t195 * t41 + t197 
     #* t59 / 0.2D1) - 0.180D3 * t3 * t85 * (t41 - t195 * t59) + t144) *
     # t161 / 0.720D3
      t213 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t212)
      t216 = -0.1D1 + x4
      t218 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t219 = t126 * t216
      t222 = log(-0.4D1 * t10 * t219)
      t223 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t225 = t222 ** 2
      t226 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t238 = t85 * t226
      t243 = t149 * t9
      t246 = log(-0.4D1 * t243 * t219)
      t258 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t259 = -t258 + t226
      t264 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t265 = x4 * t216
      t269 = log(0.4D1 * t66 * t68 * t265)
      t273 = log(-0.4D1 * t66 * t219)
      t287 = (-0.90D2 * t63 * t23 * (t218 - t222 * t223 + t225 * t226 / 
     #0.2D1) + 0.180D3 * t3 * t85 * (t223 - t222 * t226) - t33 * t238) *
     # t93 / 0.1440D4 - (0.90D2 * t63 * t23 * (t223 - t246 * t226) - 0.1
     #80D3 * t3 * t238) * t161 * t93 / 0.720D3 - t165 * t259 * t91 * t16
     #8 / 0.8D1 + (0.90D2 * t63 * t23 * (t264 - t269 * t258 - t223 + t27
     #3 * t226) + 0.180D3 * t3 * t85 * t259) * t91 * t93 / 0.1440D4
      t288 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * t216, 0.0D0,
     # t287)
      t291 = -0.1D1 + x1
      t293 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0
     #D0)
      t294 = t291 ** 2
      t295 = t12 * t294
      t299 = log(0.4D1 * t243 * t295 * x4)
      t300 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0
     #D0)
      t306 = t85 * t300
      t308 = 0.180D3 * t3 * t306
      t317 = t75 * t294
      t320 = log(0.4D1 * t173 * t317)
      t330 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0
     #D0)
      t333 = log(0.4D1 * t149 * t317)
      t335 = t333 ** 2
      t351 = -(0.90D2 * t63 * t23 * (t293 - t299 * t300) - t308) * t161 
     #* t93 / 0.720D3 - t165 * t300 * t91 * t168 / 0.8D1 - (0.90D2 * t63
     # * t23 * (t293 - t320 * t300) - t308) * t91 * t161 / 0.720D3 + (-0
     #.90D2 * t63 * t23 * (t330 - t333 * t293 + t335 * t300 / 0.2D1) + 0
     #.180D3 * t3 * t85 * (t293 - t333 * t300) - t33 * t306) * t161 / 0.
     #720D3
      t352 = FJET(XB1, XB2, s, 0.0D0, t2 * x1, 0.0D0, -t2 * t291, 0.0D0,
     # t351)
      t354 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t355 = s * t354
      t356 = t1 * x1
      t358 = t1 * t291
      t359 = t358 * x4
      t361 = t358 * t216
      t363 = t354 ** 2
      t366 = x1 * t291
      t370 = 0.1D1 / (-0.2D1 + t354)
      t371 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t374 = t363 ** 2
      t379 = log(-0.4D1 * t193 * t294 * x4 * t216 * t374)
      t381 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t387 = t3 * t19
      t388 = t23 * t370
      t401 = -(0.90D2 * t63 * t23 * (t370 * t371 - t379 * t370 * t381) -
     # 0.180D3 * t387 * t388 * t381) * t161 * t93 / 0.720D3 - t63 * t388
     # * t381 * t91 * t168 / 0.8D1
      t402 = FJET(XB1, XB2, s, 0.0D0, t355 * t356, -t355 * t359, t355 * 
     #t361, -s * t363 * t11 * t366 * x4, t401)
      t404 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t405 = s * t404
      t406 = t356 * x3
      t408 = t356 * t67
      t411 = t404 ** 2
      t417 = 0.1D1 / (-0.2D1 + t404)
      t418 = t23 * t417
      t420 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0D0)
      t425 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0D0)
      t428 = t411 ** 2
      t433 = log(-0.4D1 * t172 * t10 * t295 * t67 * t428)
      t447 = -t63 * t418 * t420 * t91 * t168 / 0.8D1 - (0.90D2 * t63 * t
     #23 * (t417 * t425 - t433 * t417 * t420) - 0.180D3 * t387 * t418 * 
     #t420) * t91 * t161 / 0.720D3
      t448 = FJET(XB1, XB2, s, t405 * t406, -t405 * t408, 0.0D0, -t405 *
     # t358, -s * t411 * t11 * t366 * x3, t447)
      t450 = KAPPA2(x1, x2, x3, x4, z)
      t451 = s * t450
      t456 = t450 ** 2
      t461 = cos(t5)
      t464 = Sqrt(x3 * t67 * t265)
      t471 = 0.1D1 / (-0.2D1 + t450)
      t474 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t479 = FJET(XB1, XB2, s, t451 * t406, -t451 * t408, -t451 * t359, 
     #t451 * t361, s * t456 * t11 * t366 * (-x3 - x4 + 0.2D1 * x3 * x4 +
     # 0.2D1 * t461 * t464), t63 * t23 * t471 * t474 * t91 * t168 / 0.8D
     #1)
      rrqq2qqht3s8e0 = t213 * t212 + t288 * t287 + t351 * t352 + t402 * 
     #t401 + t448 * t447 + t479 * 0.3141592653589793D1 * t85 * t471 * t4
     #74 * t91 * t161 * t93 / 0.8D1

      end function



      doubleprecision function rrqq2qqht3s8em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqH31J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0.
     #0D0)
      t12 = 0.3141592653589793D1 * lh
      t15 = sin(x2 * 0.3141592653589793D1)
      t16 = t15 ** 2
      t17 = z ** 2
      t18 = 0.1D1 / t17
      t19 = t16 * t18
      t20 = t1 ** 2
      t21 = t20 ** 2
      t24 = log(0.4D1 * t19 * t21)
      t25 = t24 * 0.3141592653589793D1
      t29 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t33 = 0.3141592653589793D1 ** 2
      t35 = lh ** 2
      t41 = t24 ** 2
      t46 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t50 = t4 * t7
      t51 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.0D
     #0)
      t52 = -t51 + t46
      t53 = 0.1D1 / x3
      t55 = 0.1D1 / x4
      t59 = x3 * t16
      t60 = t18 * t21
      t63 = log(0.4D1 * t59 * t60)
      t65 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.0D
     #0)
      t66 = -0.1D1 + x3
      t70 = log(-0.4D1 * t59 * t60 * t66)
      t76 = t3 * t7
      t83 = t21 * x4
      t86 = log(0.4D1 * t19 * t83)
      t94 = 0.180D3 * t12 * t76 * t46
      t100 = 0.1D1 / x1
      t104 = x1 ** 2
      t105 = t104 * t16
      t108 = log(0.4D1 * t105 * t60)
      t121 = t4 * t7 * t8 / 0.16D2 + (-0.180D3 * t12 - 0.90D2 * t25) * t
     #3 * t7 * t29 / 0.1440D4 + (0.3141592653589793D1 * (-0.30D2 * t33 +
     # 0.180D3 * t35) + 0.180D3 * t25 * lh + 0.45D2 * t41 * 0.3141592653
     #589793D1) * t3 * t7 * t46 / 0.1440D4 + t50 * t52 * t53 * t55 / 0.1
     #6D2 + (0.90D2 * t4 * t7 * (t29 - t63 * t46 - t65 + t70 * t51) - 0.
     #180D3 * t12 * t76 * t52) * t53 / 0.1440D4 + (0.90D2 * t4 * t7 * (t
     #29 - t86 * t46) - t94) * t55 / 0.1440D4 + t50 * t52 * t53 * t100 /
     # 0.8D1 + (0.90D2 * t4 * t7 * (t29 - t108 * t46) - t94) * t100 / 0.
     #720D3 + t50 * t46 * t100 * t55 / 0.8D1
      t122 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t121)
      t125 = -0.1D1 + x4
      t127 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t131 = log(-0.4D1 * t19 * t83 * t125)
      t132 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t148 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t154 = (-0.90D2 * t4 * t7 * (t127 - t131 * t132) + 0.180D3 * t12 *
     # t76 * t132) * t55 / 0.1440D4 - t50 * t132 * t100 * t55 / 0.8D1 + 
     #t50 * (-t132 + t148) * t53 * t55 / 0.16D2
      t155 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * t125, 0.0D0,
     # t154)
      t158 = -0.1D1 + x1
      t160 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0
     #D0)
      t165 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0
     #D0)
      t166 = t158 ** 2
      t170 = log(0.4D1 * t105 * t60 * t166)
      t186 = -t50 * t160 * t53 * t100 / 0.8D1 + (-0.90D2 * t4 * t7 * (t1
     #65 - t170 * t160) + 0.180D3 * t12 * t76 * t160) * t100 / 0.720D3 -
     # t50 * t160 * t100 * t55 / 0.8D1
      t187 = FJET(XB1, XB2, s, 0.0D0, t2 * x1, 0.0D0, -t2 * t158, 0.0D0,
     # t186)
      t189 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t190 = s * t189
      t191 = t1 * x1
      t193 = t1 * t158
      t198 = t189 ** 2
      t201 = x1 * t158
      t206 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t209 = 0.1D1 / (-0.2D1 + t189) * t206 * t100 * t55
      t212 = FJET(XB1, XB2, s, 0.0D0, t190 * t191, -t190 * t193 * x4, t1
     #90 * t193 * t125, -s * t198 * t20 * t201 * x4, -t50 * t209 / 0.8D1
     #)
      t217 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t218 = s * t217
      t224 = t217 ** 2
      t231 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0D0)
      t234 = 0.1D1 / (-0.2D1 + t217) * t231 * t53 * t100
      t237 = FJET(XB1, XB2, s, t218 * t191 * x3, -t218 * t191 * t66, 0.0
     #D0, -t218 * t193, -s * t224 * t20 * t201 * x3, -t50 * t234 / 0.8D1
     #)
      rrqq2qqht3s8em1 = t122 * t121 + t155 * t154 + t187 * t186 - t212 *
     # 0.3141592653589793D1 * t76 * t209 / 0.8D1 - t237 * 0.314159265358
     #9793D1 * t76 * t234 / 0.8D1

      end function



      doubleprecision function rrqq2qqht3s8em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0.
     #0D0)
      t9 = t7 * t8
      t10 = 0.1D1 / x4
      t14 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, 0.0D
     #0)
      t21 = 0.1D1 / x1
      t25 = rrqq2qqH31J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0
     #.0D0)
      t32 = sin(x2 * 0.3141592653589793D1)
      t33 = t32 ** 2
      t34 = z ** 2
      t37 = t1 ** 2
      t38 = t37 ** 2
      t41 = log(0.4D1 * t33 / t34 * t38)
      t48 = t4 * t9 * t10 / 0.16D2 + t4 * t7 * (-t14 + t8) / x3 / 0.16D2
     # + t4 * t9 * t21 / 0.8D1 + t4 * t7 * t25 / 0.16D2 + (-0.180D3 * 0.
     #3141592653589793D1 * lh - 0.90D2 * t41 * 0.3141592653589793D1) * t
     #3 * t9 / 0.1440D4
      t49 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t48)
      t54 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t56 = t7 * t54 * t10
      t59 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2 * x4, -t2 * (-0.1D1 + x4)
     #, 0.0D0, -t4 * t56 / 0.16D2)
      t67 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0D
     #0)
      t69 = t7 * t67 * t21
      t72 = FJET(XB1, XB2, s, 0.0D0, t2 * x1, 0.0D0, -t2 * (-0.1D1 + x1)
     #, 0.0D0, -t4 * t69 / 0.8D1)
      rrqq2qqht3s8em2 = t49 * t48 - t59 * 0.3141592653589793D1 * t3 * t5
     #6 / 0.16D2 - t72 * 0.3141592653589793D1 * t3 * t69 / 0.8D1

      end function



      doubleprecision function rrqq2qqht3s8em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      t1 = z - 0.1D1
      t3 = 0.1D1 / t1
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqH31J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 0.
     #0D0)
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, s * t1, 0.0D0, 0.3141
     #592653589793D1 * t3 * t7 * t8 / 0.16D2)
      rrqq2qqht3s8em3 = t12 * 0.3141592653589793D1 * t3 * t7 * t8 / 0.16
     #D2

      end function



      doubleprecision function rrqq2qqht3s8em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqq2qqH31J1
      doubleprecision rrqq2qqH31J2
      doubleprecision rrqq2qqH31J3
      rrqq2qqht3s8em4 = 0.0D0

      end function
  
 

      doubleprecision function rrqq2qqH31J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = t1 ** 2
      t5 = (0.1D1 - z) ** 2
      t7 = 0.1D1 - x1
      t13 = cos(x2 * 0.3141592653589793D1)
      t19 = Sqrt(x3 * (0.1D1 - x3) * x4 * (0.1D1 - x4))
      t22 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t13 * t19
      t24 = s ** 2
      t25 = t2 ** 2
      t27 = t5 ** 2
      t29 = x1 ** 2
      t30 = t7 ** 2
      t32 = t22 ** 2
      rrqq2qqH31J1 = 0.16D2 / 0.27D2 * s * t2 * t5 * x1 * t7 * t22 * wd 
     #* (-t24 - t24 * t25 * t27 * t29 * t30 * t32) / z / 0.3141592653589
     #793D1

      end function
  
   
 

      doubleprecision function rrqq2qqH31J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = t1 ** 2
      t5 = (0.1D1 - z) ** 2
      t7 = 0.1D1 - x1
      t13 = cos(x2 * 0.3141592653589793D1)
      t14 = 0.1D1 - x3
      t16 = 0.1D1 - x4
      t19 = Sqrt(x3 * t14 * x4 * t16)
      t22 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t13 * t19
      t24 = s ** 2
      t26 = t24 * t2 * t5
      t31 = t7 ** 2
      t35 = x1 ** 2
      t39 = t2 ** 2
      t41 = t5 ** 2
      t44 = t22 ** 2
      rrqq2qqH31J2 = 0.16D2 / 0.27D2 * s * t2 * t5 * x1 * t7 * t22 * wd 
     #* (-t26 * x1 * t14 * t7 * t16 - t26 * t31 * x4 * t16 - t26 * t35 *
     # t14 * x3 + t24 * t39 * t41 * t35 * t31 * t44 - t26 * t7 * x4 * x1
     # * x3 + t24 + 0.2D1 * t26 * x1 * t7 * t22) / z / 0.314159265358979
     #3D1

      end function
  
   
 

      doubleprecision function rrqq2qqH31J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = t1 ** 2
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t7 = 0.1D1 - x1
      t13 = cos(x2 * 0.3141592653589793D1)
      t14 = 0.1D1 - x3
      t16 = 0.1D1 - x4
      t19 = Sqrt(x3 * t14 * x4 * t16)
      t22 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t13 * t19
      t24 = s ** 2
      t25 = t24 * t1
      t26 = t4 * t7
      t30 = t24 * t2 * t5
      t38 = t24 * t2 * t1 * t5 * t4
      t39 = x1 ** 2
      t40 = t39 * t14
      t41 = t7 * t22
      t44 = t7 ** 2
      t45 = t44 * x4
      t46 = x1 * t22
      t53 = t2 ** 2
      t55 = t5 ** 2
      t58 = t22 ** 2
      t73 = t4 * x1
      t82 = -t25 * t26 * x4 + 0.2D1 * t30 * x1 * t7 * t22 - t38 * t40 * 
     #t41 - t38 * t45 * t46 + t24 + t30 * x1 * t14 * t7 * t16 + t24 * t5
     #3 * t55 * t39 * t44 * t58 - t38 * t44 * t16 * t46 - t25 * t26 * t1
     #6 - t38 * t39 * x3 * t41 + t30 * t45 * t16 + t30 * t40 * x3 - t25 
     #* t73 * t14 + t30 * t7 * x4 * x1 * x3 - t25 * t73 * x3
      rrqq2qqH31J3 = 0.16D2 / 0.27D2 * s * t2 * t5 * x1 * t7 * t22 * wd 
     #* t82 / z / 0.3141592653589793D1

      end function
  
 