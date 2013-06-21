  
      subroutine rrqg2qght8
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrqg2qgh81J1  
      doubleprecision rrqg2qgh81J2  
      doubleprecision rrqg2qgh81J3  
      doubleprecision rrqg2qgh81J4  
      doubleprecision rrqg2qgh81J5  
      doubleprecision rrqg2qgh81J6  
      doubleprecision rrqg2qgh82J1  
      doubleprecision rrqg2qgh82J2  
      doubleprecision rrqg2qgh82J3  
      doubleprecision rrqg2qgh82J4  
      doubleprecision rrqg2qgh82J5  
      doubleprecision rrqg2qgh82J6  
      doubleprecision rrqg2qgh82J7  
      doubleprecision rrqg2qgh83J1  
      doubleprecision rrqg2qgh83J2  
      doubleprecision rrqg2qgh83J3  
      doubleprecision rrqg2qgh83J4  
      doubleprecision rrqg2qgh83J5  
      doubleprecision rrqg2qgh83J6  
      doubleprecision rrqg2qgh83J7  
      doubleprecision rrqg2qgh84J1  
      doubleprecision rrqg2qgh84J2  
      doubleprecision rrqg2qgh84J3  
      doubleprecision rrqg2qgh84J4  
      doubleprecision rrqg2qgh84J5  
      doubleprecision rrqg2qgh84J6  
      doubleprecision rrqg2qght8s1e1  
      doubleprecision rrqg2qght8s1e0  
      doubleprecision rrqg2qght8s1em1  
      doubleprecision rrqg2qght8s1em2  
      doubleprecision rrqg2qght8s1em3  
      doubleprecision rrqg2qght8s1em4  
      doubleprecision rrqg2qght8s2e1  
      doubleprecision rrqg2qght8s2e0  
      doubleprecision rrqg2qght8s2em1  
      doubleprecision rrqg2qght8s2em2  
      doubleprecision rrqg2qght8s2em3  
      doubleprecision rrqg2qght8s2em4  
      doubleprecision rrqg2qght8s3e1  
      doubleprecision rrqg2qght8s3e0  
      doubleprecision rrqg2qght8s3em1  
      doubleprecision rrqg2qght8s3em2  
      doubleprecision rrqg2qght8s3em3  
      doubleprecision rrqg2qght8s3em4  
      doubleprecision rrqg2qght8s4e1  
      doubleprecision rrqg2qght8s4e0  
      doubleprecision rrqg2qght8s4em1  
      doubleprecision rrqg2qght8s4em2  
      doubleprecision rrqg2qght8s4em3  
      doubleprecision rrqg2qght8s4em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrqg2qght8s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght8s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrqg2qght8s3e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrqg2qght8s4e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrqg2qght8s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght8s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrqg2qght8s3e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrqg2qght8s4e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrqg2qght8s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght8s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrqg2qght8s3em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrqg2qght8s4em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrqg2qght8s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght8s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrqg2qght8s3em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrqg2qght8s4em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrqg2qght8s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght8s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrqg2qght8s3em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrqg2qght8s4em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrqg2qght8s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght8s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrqg2qght8s3em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrqg2qght8s4em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrqg2qght8s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = rrqg2qgh83J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.10
     #D1, x4)
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
      t23 = t22 * t3
      t24 = t5 * lh
      t27 = 0.3141592653589793D1 ** 2
      t30 = lh ** 2
      t33 = 0.60D2 * lh * t27 - 0.2884936567583026D3 - 0.120D3 * t30 * l
     #h
      t36 = t22 * t21 * t3
      t39 = t21 * t3
      t42 = 0.180D3 * t30 - 0.30D2 * t27
      t43 = t5 * t42
      t46 = (-0.90D2 * t23 * t24 + t6 * t33 - 0.15D2 * t36 * t5 - t39 * 
     #t43) * 0.3141592653589793D1
      t47 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t54 = t6 * t42
      t56 = (0.180D3 * t39 * t24 + 0.45D2 * t23 * t5 + t54) * 0.31415926
     #53589793D1
      t57 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t60 = t6 * lh
      t65 = (-0.180D3 * t60 - 0.90D2 * t39 * t5) * 0.3141592653589793D1
      t66 = rrqg2qgh83J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t75 = t27 ** 2
      t76 = t30 ** 2
      t83 = t22 ** 2
      t88 = (0.30D2 * t36 * t24 + t23 * t43 / 0.2D1 - t39 * t5 * t33 + t
     #6 * (t75 + 0.60D2 * t76 + 0.5769873135166051D3 * lh - 0.60D2 * t30
     # * t27) + 0.15D2 / 0.4D1 * t83 * t3 * t5) * 0.3141592653589793D1
      t89 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t92 = z * t57
      t93 = x1 ** 2
      t94 = x3 * t93
      t95 = t94 * t15
      t96 = t12 * t18
      t97 = -0.1D1 + x3
      t98 = 0.1D1 / t97
      t99 = t96 * t98
      t102 = log(-0.4D1 * t95 * t99)
      t103 = t102 * z
      t105 = t102 ** 2
      t106 = t105 * z
      t110 = cos(t13)
      t112 = Sqrt(-x3 * t97)
      t117 = 0.1D1 / (-x3 - z + 0.2D1 * t110 * t112 * z)
      t121 = log(0.4D1 * t94 * t19)
      t123 = t121 ** 2
      t130 = 0.3141592653589793D1 * lh
      t131 = z * t47
      t140 = t42 * 0.3141592653589793D1
      t143 = -z * t89 * t117 - t89
      t145 = t6 * t140 * t143
      t147 = 0.1D1 / x3
      t149 = 0.1D1 / x1
      t152 = t93 * t15
      t155 = log(0.4D1 * t152 * t96)
      t160 = t155 ** 2
      t163 = t160 * t155
      t171 = t33 * 0.3141592653589793D1
      t173 = t6 * t171 * t89
      t184 = x2 ** 2
      t185 = x3 * t184
      t186 = t185 * t93
      t191 = log(-0.4D1 * t186 * t16 * t18 * t98)
      t192 = t191 * z
      t198 = log(0.4D1 * t186 * t19)
      t209 = 0.1D1 / x2
      t210 = t209 * t149
      t213 = t184 * t93
      t216 = log(0.4D1 * t213 * t19)
      t218 = t216 ** 2
      t231 = t6 * t140 * t89
      t243 = x3 * t15
      t246 = log(-0.4D1 * t243 * t99)
      t251 = log(0.4D1 * t243 * t96)
      t252 = t246 * z * t117 + t251
      t255 = t251 ** 2
      t257 = t246 ** 2
      t262 = t255 * t251 / 0.6D1 + t257 * t246 * z * t117 / 0.6D1
      t276 = -z * t117 - 0.1D1
      t288 = -t257 * z * t117 / 0.2D1 - t255 / 0.2D1
      t295 = log(0.4D1 * t185 * t19)
      t297 = t185 * t15
      t300 = log(-0.4D1 * t297 * t99)
      t301 = t300 * z
      t303 = t300 ** 2
      t304 = t303 * z
      t309 = t295 ** 2
      t328 = t184 * t15
      t331 = log(0.4D1 * t328 * t96)
      t337 = t331 ** 2
      t340 = t337 * t331
      t357 = t6 * 0.3141592653589793D1 * t7 / 0.32D2 + t46 * t47 / 0.288
     #0D4 + t56 * t57 / 0.2880D4 + t65 * t66 / 0.2880D4 + t88 * t89 / 0.
     #2880D4 - (0.90D2 * t6 * 0.3141592653589793D1 * (-t57 + (-t92 + t10
     #3 * t47 - t106 * t89 / 0.2D1) * t117 + t121 * t47 - t123 * t89 / 0
     #.2D1) - 0.180D3 * t6 * t130 * ((-t131 + t103 * t89) * t117 - t47 +
     # t121 * t89) + t145) * t147 * t149 / 0.1440D4 - (t6 * t140 * (-t47
     # + t155 * t89) + 0.90D2 * t6 * 0.3141592653589793D1 * (-t66 - t160
     # * t47 / 0.2D1 + t163 * t89 / 0.6D1 + t155 * t57) - t173 - 0.180D3
     # * t6 * t130 * (-t57 - t160 * t89 / 0.2D1 + t155 * t47)) * t149 / 
     #0.1440D4 - (0.90D2 * t6 * 0.3141592653589793D1 * (-t47 + (-t131 + 
     #t192 * t89) * t117 + t198 * t89) - 0.180D3 * t6 * t130 * t143) * t
     #147 * t210 / 0.720D3 + (0.90D2 * t6 * 0.3141592653589793D1 * (t57 
     #- t216 * t47 + t218 * t89 / 0.2D1) - 0.180D3 * t6 * t130 * (t47 - 
     #t216 * t89) + t231) * t209 * t149 / 0.720D3 + ((-0.90D2 * t6 * 0.3
     #141592653589793D1 * t57 + 0.180D3 * t6 * t130 * t47 - t231) * t252
     # - 0.90D2 * t6 * 0.3141592653589793D1 * t89 * t262 + (-t6 * t140 *
     # t47 - 0.90D2 * t6 * 0.3141592653589793D1 * t66 - t173 + 0.180D3 *
     # t6 * t130 * t57) * t276 + (-0.90D2 * t6 * 0.3141592653589793D1 * 
     #t47 + 0.180D3 * t6 * t130 * t89) * t288) * t147 / 0.2880D4 - (0.90
     #D2 * t6 * 0.3141592653589793D1 * (-t57 + t295 * t47 + (-t92 + t301
     # * t47 - t304 * t89 / 0.2D1) * t117 - t309 * t89 / 0.2D1) - 0.180D
     #3 * t6 * t130 * ((-t131 + t301 * t89) * t117 - t47 + t295 * t89) +
     # t145) * t147 * t209 / 0.1440D4 + (t6 * t140 * (t47 - t331 * t89) 
     #+ 0.90D2 * t6 * 0.3141592653589793D1 * (-t331 * t57 + t337 * t47 /
     # 0.2D1 - t340 * t89 / 0.6D1 + t66) + t173 - 0.180D3 * t6 * t130 * 
     #(t57 - t331 * t47 + t337 * t89 / 0.2D1)) * t209 / 0.1440D4
      t358 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t357)
      t360 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t363 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t364 = z * t363
      t365 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t376 = z * t365
      t387 = -t360 - z * t360 * t117
      t389 = t6 * t140 * t387
      t400 = rrqg2qgh81J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t409 = t6 * t171 * t360
      t420 = rrqg2qgh81J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t439 = t6 * t140 * t360
      t545 = -(0.90D2 * t6 * 0.3141592653589793D1 * (-t309 * t360 / 0.2D
     #1 - t363 + (-t364 + t301 * t365 - t304 * t360 / 0.2D1) * t117 + t2
     #95 * t365) - 0.180D3 * t6 * t130 * ((-t376 + t301 * t360) * t117 +
     # t295 * t360 - t365) + t389) * t147 * t209 / 0.1440D4 + (t6 * t140
     # * (-t331 * t360 + t365) + 0.90D2 * t6 * 0.3141592653589793D1 * (-
     #t340 * t360 / 0.6D1 + t400 + t337 * t365 / 0.2D1 - t331 * t363) + 
     #t409 - 0.180D3 * t6 * t130 * (t363 - t331 * t365 + t337 * t360 / 0
     #.2D1)) * t209 / 0.1440D4 + t6 * 0.3141592653589793D1 * t420 / 0.32
     #D2 + t46 * t365 / 0.2880D4 + t56 * t363 / 0.2880D4 + t65 * t400 / 
     #0.2880D4 + t88 * t360 / 0.2880D4 + ((-0.90D2 * t6 * 0.314159265358
     #9793D1 * t363 + 0.180D3 * t6 * t130 * t365 - t439) * t252 - 0.90D2
     # * t6 * 0.3141592653589793D1 * t360 * t262 + (-t6 * t140 * t365 - 
     #0.90D2 * t6 * 0.3141592653589793D1 * t400 - t409 + 0.180D3 * t6 * 
     #t130 * t363) * t276 + (-0.90D2 * t6 * 0.3141592653589793D1 * t365 
     #+ 0.180D3 * t6 * t130 * t360) * t288) * t147 / 0.2880D4 - (0.90D2 
     #* t6 * 0.3141592653589793D1 * (-t363 + (-t364 + t103 * t365 - t106
     # * t360 / 0.2D1) * t117 + t121 * t365 - t123 * t360 / 0.2D1) - 0.1
     #80D3 * t6 * t130 * ((-t376 + t103 * t360) * t117 + t121 * t360 - t
     #365) + t389) * t147 * t149 / 0.1440D4 - (t6 * t140 * (-t365 + t155
     # * t360) + 0.90D2 * t6 * 0.3141592653589793D1 * (-t400 - t160 * t3
     #65 / 0.2D1 + t163 * t360 / 0.6D1 + t155 * t363) - t409 - 0.180D3 *
     # t6 * t130 * (t155 * t365 - t363 - t160 * t360 / 0.2D1)) * t149 / 
     #0.1440D4 - (0.90D2 * t6 * 0.3141592653589793D1 * ((-t376 + t192 * 
     #t360) * t117 - t365 + t198 * t360) - 0.180D3 * t6 * t130 * t387) *
     # t147 * t210 / 0.720D3 + (0.90D2 * t6 * 0.3141592653589793D1 * (t2
     #18 * t360 / 0.2D1 - t216 * t365 + t363) - 0.180D3 * t6 * t130 * (t
     #365 - t216 * t360) + t439) * t209 * t149 / 0.720D3
      t546 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t545)
      t548 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t550 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t551 = z * t550
      t553 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t564 = z * t548
      t575 = -z * t553 * t117 - t553
      t577 = t6 * t140 * t575
      t590 = rrqg2qgh84J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t597 = t6 * t171 * t553
      t636 = t6 * t140 * t553
      t688 = rrqg2qgh84J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t733 = -(0.90D2 * t6 * 0.3141592653589793D1 * (t121 * t548 - t550 
     #+ (-t551 + t103 * t548 - t106 * t553 / 0.2D1) * t117 - t123 * t553
     # / 0.2D1) - 0.180D3 * t6 * t130 * ((-t564 + t103 * t553) * t117 + 
     #t121 * t553 - t548) + t577) * t147 * t149 / 0.1440D4 - (t6 * t140 
     #* (t155 * t553 - t548) + 0.90D2 * t6 * 0.3141592653589793D1 * (-t1
     #60 * t548 / 0.2D1 + t163 * t553 / 0.6D1 - t590 + t155 * t550) - t5
     #97 - 0.180D3 * t6 * t130 * (-t550 - t160 * t553 / 0.2D1 + t155 * t
     #548)) * t149 / 0.1440D4 - (0.90D2 * t6 * 0.3141592653589793D1 * (t
     #198 * t553 + (-t564 + t192 * t553) * t117 - t548) - 0.180D3 * t6 *
     # t130 * t575) * t147 * t210 / 0.720D3 + (0.90D2 * t6 * 0.314159265
     #3589793D1 * (-t216 * t548 + t550 + t218 * t553 / 0.2D1) - 0.180D3 
     #* t6 * t130 * (t548 - t216 * t553) + t636) * t209 * t149 / 0.720D3
     # - (0.90D2 * t6 * 0.3141592653589793D1 * (-t309 * t553 / 0.2D1 + (
     #-t551 + t301 * t548 - t304 * t553 / 0.2D1) * t117 + t295 * t548 - 
     #t550) - 0.180D3 * t6 * t130 * ((-t564 + t301 * t553) * t117 + t295
     # * t553 - t548) + t577) * t147 * t209 / 0.1440D4 + (t6 * t140 * (-
     #t331 * t553 + t548) + 0.90D2 * t6 * 0.3141592653589793D1 * (-t340 
     #* t553 / 0.6D1 + t590 - t331 * t550 + t337 * t548 / 0.2D1) + t597 
     #- 0.180D3 * t6 * t130 * (-t331 * t548 + t550 + t337 * t553 / 0.2D1
     #)) * t209 / 0.1440D4 + t6 * 0.3141592653589793D1 * t688 / 0.32D2 +
     # t46 * t548 / 0.2880D4 + t56 * t550 / 0.2880D4 + t65 * t590 / 0.28
     #80D4 + ((-0.90D2 * t6 * 0.3141592653589793D1 * t550 + 0.180D3 * t6
     # * t130 * t548 - t636) * t252 - 0.90D2 * t6 * 0.3141592653589793D1
     # * t553 * t262 + (-t6 * t140 * t548 - 0.90D2 * t6 * 0.314159265358
     #9793D1 * t590 - t597 + 0.180D3 * t6 * t130 * t550) * t276 + (-0.90
     #D2 * t6 * 0.3141592653589793D1 * t548 + 0.180D3 * t6 * t130 * t553
     #) * t288) * t147 / 0.2880D4 + t88 * t553 / 0.2880D4
      t734 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t733)
      t736 = t2 * x1
      t737 = -0.1D1 + x1
      t738 = x1 * z
      t739 = 0.1D1 - x1 + t738
      t740 = 0.1D1 / t739
      t742 = t2 * t737 * t740
      t743 = s * t17
      t745 = x1 * t737 * t740
      t746 = t743 * t745
      t747 = -t737
      t748 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, t747, 0.10D1, 0.10
     #D1, x4)
      t751 = t18 * t740
      t752 = t737 ** 2
      t757 = log(-0.4D1 * t94 * t16 * t751 * t752 * t98)
      t758 = t757 * z
      t759 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, t747, 0.10D1, 0.10
     #D1, x4)
      t761 = t757 ** 2
      t762 = t761 * z
      t763 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, t747, 0.10D1, 0.10
     #D1, x4)
      t768 = x3 * t739
      t770 = Sqrt(-t768 * t97)
      t774 = x3 * x1
      t775 = t774 * z
      t776 = 0.3D1 * t775
      t777 = x1 * t11
      t778 = x3 * t11
      t779 = t778 * x1
      t781 = 0.2D1 * t94 * z
      t782 = t94 * t11
      t783 = 0.2D1 * t774
      t784 = -z + 0.2D1 * t110 * t770 * z + t738 - t776 - t777 + t779 + 
     #t781 - t782 - x3 + t783 - t94
      t785 = 0.1D1 / t784
      t787 = t740 * t752
      t788 = t96 * t787
      t791 = log(0.4D1 * t95 * t788)
      t793 = t791 ** 2
      t800 = z * t759
      t811 = t739 * t785
      t813 = z * t763 * t811 + t763
      t820 = t152 * t12
      t821 = t751 * t752
      t824 = log(0.4D1 * t820 * t821)
      t829 = t824 ** 2
      t832 = t829 * t824
      t835 = rrqg2qgh81J4(s, XB1, XB2, z, lh, wd, nf, t747, 0.10D1, 0.10
     #D1, x4)
      t853 = t185 * t152
      t856 = log(0.4D1 * t853 * t788)
      t862 = log(-0.4D1 * t853 * t96 * t787 * t98)
      t863 = t862 * z
      t879 = t213 * t15
      t882 = log(0.4D1 * t879 * t788)
      t883 = t882 ** 2
      t902 = -(0.90D2 * t6 * 0.3141592653589793D1 * (t748 + (z * t748 - 
     #t758 * t759 + t762 * t763 / 0.2D1) * t739 * t785 - t791 * t759 + t
     #793 * t763 / 0.2D1) - 0.180D3 * t6 * t130 * ((t800 - t758 * t763) 
     #* t739 * t785 - t791 * t763 + t759) + t6 * t140 * t813) * t147 * t
     #149 / 0.1440D4 - (t6 * t140 * (-t824 * t763 + t759) + 0.90D2 * t6 
     #* 0.3141592653589793D1 * (t829 * t759 / 0.2D1 - t832 * t763 / 0.6D
     #1 + t835 - t824 * t748) + t6 * t171 * t763 - 0.180D3 * t6 * t130 *
     # (-t824 * t759 + t829 * t763 / 0.2D1 + t748)) * t149 / 0.1440D4 - 
     #(0.90D2 * t6 * 0.3141592653589793D1 * (-t856 * t763 + t759 + (t800
     # - t863 * t763) * t739 * t785) - 0.180D3 * t6 * t130 * t813) * t14
     #7 * t210 / 0.720D3 + (0.90D2 * t6 * 0.3141592653589793D1 * (-t883 
     #* t763 / 0.2D1 - t748 + t882 * t759) - 0.180D3 * t6 * t130 * (-t75
     #9 + t882 * t763) - t6 * t140 * t763) * t209 * t149 / 0.720D3
      t903 = FJET(XB1, XB2, s, 0.0D0, t736, -t742, 0.0D0, -t746, t902)
      t905 = x2 * s
      t906 = t905 * t1
      t907 = -0.1D1 + x2
      t908 = t907 * s
      t909 = t908 * t1
      t910 = -t907
      t911 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t910, 0.10
     #D1, x4)
      t912 = t96 * t907
      t915 = log(-0.4D1 * t297 * t912)
      t916 = t915 ** 2
      t917 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t910, 0.10
     #D1, x4)
      t920 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t910, 0.10
     #D1, x4)
      t932 = t6 * t140 * t917
      t939 = log(-0.4D1 * t328 * t912)
      t944 = t939 ** 2
      t945 = t944 * t939
      t949 = rrqg2qgh83J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t910, 0.10
     #D1, x4)
      t968 = t18 * t907
      t972 = log(-0.4D1 * t186 * t16 * t968)
      t987 = log(-0.4D1 * t879 * t912)
      t989 = t987 ** 2
      t1005 = -(0.90D2 * t6 * 0.3141592653589793D1 * (t911 + t916 * t917
     # / 0.2D1 - t915 * t920) - 0.180D3 * t6 * t130 * (-t915 * t917 + t9
     #20) + t932) * t147 * t209 / 0.1440D4 + (t6 * t140 * (-t920 + t939 
     #* t917) + 0.90D2 * t6 * 0.3141592653589793D1 * (t945 * t917 / 0.6D
     #1 + t939 * t911 - t949 - t944 * t920 / 0.2D1) - t6 * t171 * t917 -
     # 0.180D3 * t6 * t130 * (-t911 - t944 * t917 / 0.2D1 + t939 * t920)
     #) * t209 / 0.1440D4 - (0.90D2 * t6 * 0.3141592653589793D1 * (-t972
     # * t917 + t920) - 0.180D3 * t6 * t130 * t917) * t147 * t210 / 0.72
     #0D3 + (0.90D2 * t6 * 0.3141592653589793D1 * (t987 * t920 - t989 * 
     #t917 / 0.2D1 - t911) - 0.180D3 * t6 * t130 * (t987 * t917 - t920) 
     #- t932) * t209 * t149 / 0.720D3
      t1006 = FJET(XB1, XB2, s, 0.0D0, t906, 0.0D0, -t909, 0.0D0, t1005)
      t1008 = x2 * x3
      t1011 = Sqrt(x3 * t907 * t97)
      t1012 = t110 * t1011
      t1014 = 0.2D1 * t1012 * x2
      t1016 = 0.1D1 - x3 + t1008
      t1017 = 0.1D1 / t1016
      t1019 = t2 * (0.1D1 - x3 - x2 + t1008 + t185 + t1014) * t1017
      t1024 = t2 * x2 * (-0.1D1 + t1008 + 0.2D1 * t1012) * t1017
      t1025 = x2 * z
      t1026 = t1025 - z - x2
      t1027 = t97 * t1017
      t1028 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t910, -t1
     #027, x4)
      t1029 = t1026 * t1028
      t1030 = t907 * t97
      t1031 = t1016 ** 2
      t1032 = 0.1D1 / t1031
      t1033 = t1030 * t1032
      t1037 = log(0.4D1 * t853 * t96 * t1033)
      t1038 = t1037 * t1026
      t1039 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t910, -t1
     #027, x4)
      t1043 = t1008 * z
      t1044 = t185 * z
      t1050 = 0.1D1 / (-t185 + x3 - t1025 + z + x2 - t1043 + t1044 - t10
     #14 - 0.2D1 * t1012 * z + 0.2D1 * t1012 * t1025)
      t1054 = 0.3141592653589793D1 * t1026
      t1056 = t1054 * t1039 * t1050
      t1063 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t910, -t1
     #027, x4)
      t1070 = log(0.4D1 * t185 * t16 * t968 * t97 * t1032)
      t1071 = t1070 * t1026
      t1073 = t1070 ** 2
      t1074 = t1073 * t1026
      t1093 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (-t1029 + t1038 * 
     #t1039) * t1050 - 0.180D3 * t60 * t1056) * t147 * t210 / 0.720D3 - 
     #(-0.90D2 * t6 * 0.3141592653589793D1 * (-t1026 * t1063 + t1071 * t
     #1028 - t1074 * t1039 / 0.2D1) * t1050 + 0.180D3 * t60 * 0.31415926
     #53589793D1 * (-t1029 + t1071 * t1039) * t1050 + t54 * t1056) * t14
     #7 * t209 / 0.1440D4
      t1094 = FJET(XB1, XB2, s, 0.0D0, t1019, 0.0D0, -t1024, 0.0D0, t109
     #3)
      t1096 = t1 * t737
      t1098 = t908 * t1096 * t740
      t1099 = t905 * t1096
      t1101 = t743 * t907 * t745
      t1102 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, t747, t910, 0.10D
     #1, x4)
      t1107 = log(-0.4D1 * t853 * t96 * t787 * t907)
      t1108 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, t747, t910, 0.10D
     #1, x4)
      t1125 = log(-0.4D1 * t213 * t16 * t751 * t752 * t907)
      t1127 = t1125 ** 2
      t1130 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, t747, t910, 0.10D
     #1, x4)
      t1146 = -(0.90D2 * t6 * 0.3141592653589793D1 * (-t1102 + t1107 * t
     #1108) + 0.180D3 * t6 * t130 * t1108) * t147 * t210 / 0.720D3 + (0.
     #90D2 * t6 * 0.3141592653589793D1 * (-t1125 * t1102 + t1127 * t1108
     # / 0.2D1 + t1130) - 0.180D3 * t6 * t130 * (t1102 - t1125 * t1108) 
     #+ t6 * t140 * t1108) * t209 * t149 / 0.720D3
      t1147 = FJET(XB1, XB2, s, 0.0D0, t1098, t736, -t1099, t1101, t1146
     #)
      t1149 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t910, 0.1
     #0D1, x4)
      t1151 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t910, 0.1
     #0D1, x4)
      t1165 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t910, 0.1
     #0D1, x4)
      t1177 = t6 * t140 * t1149
      t1202 = rrqg2qgh84J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t910, 0.1
     #0D1, x4)
      t1224 = -(0.90D2 * t6 * 0.3141592653589793D1 * (-t972 * t1149 + t1
     #151) - 0.180D3 * t6 * t130 * t1149) * t147 * t210 / 0.720D3 + (0.9
     #0D2 * t6 * 0.3141592653589793D1 * (-t989 * t1149 / 0.2D1 - t1165 +
     # t987 * t1151) - 0.180D3 * t6 * t130 * (-t1151 + t987 * t1149) - t
     #1177) * t209 * t149 / 0.720D3 - (0.90D2 * t6 * 0.3141592653589793D
     #1 * (-t915 * t1151 + t916 * t1149 / 0.2D1 + t1165) - 0.180D3 * t6 
     #* t130 * (t1151 - t915 * t1149) + t1177) * t147 * t209 / 0.1440D4 
     #+ (t6 * t140 * (-t1151 + t939 * t1149) + 0.90D2 * t6 * 0.314159265
     #3589793D1 * (-t1202 - t944 * t1151 / 0.2D1 + t945 * t1149 / 0.6D1 
     #+ t939 * t1165) - t6 * t171 * t1149 - 0.180D3 * t6 * t130 * (-t116
     #5 - t944 * t1149 / 0.2D1 + t939 * t1151)) * t209 / 0.1440D4
      t1225 = FJET(XB1, XB2, s, 0.0D0, -t909, 0.0D0, t906, 0.0D0, t1224)
      t1227 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, t747, 0.10D1, 0.1
     #0D1, x4)
      t1229 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, t747, 0.10D1, 0.1
     #0D1, x4)
      t1231 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, t747, 0.10D1, 0.1
     #0D1, x4)
      t1244 = z * t1229
      t1256 = z * t1231 * t811 + t1231
      t1269 = rrqg2qgh84J4(s, XB1, XB2, z, lh, wd, nf, t747, 0.10D1, 0.1
     #0D1, x4)
      t1323 = -(0.90D2 * t6 * 0.3141592653589793D1 * ((z * t1227 - t758 
     #* t1229 + t762 * t1231 / 0.2D1) * t739 * t785 + t1227 + t793 * t12
     #31 / 0.2D1 - t791 * t1229) - 0.180D3 * t6 * t130 * ((t1244 - t758 
     #* t1231) * t739 * t785 - t791 * t1231 + t1229) + t6 * t140 * t1256
     #) * t147 * t149 / 0.1440D4 - (t6 * t140 * (t1229 - t824 * t1231) +
     # 0.90D2 * t6 * 0.3141592653589793D1 * (t829 * t1229 / 0.2D1 + t126
     #9 - t832 * t1231 / 0.6D1 - t824 * t1227) + t6 * t171 * t1231 - 0.1
     #80D3 * t6 * t130 * (t1227 - t824 * t1229 + t829 * t1231 / 0.2D1)) 
     #* t149 / 0.1440D4 - (0.90D2 * t6 * 0.3141592653589793D1 * (-t856 *
     # t1231 + t1229 + (t1244 - t863 * t1231) * t739 * t785) - 0.180D3 *
     # t6 * t130 * t1256) * t147 * t210 / 0.720D3 + (0.90D2 * t6 * 0.314
     #1592653589793D1 * (-t883 * t1231 / 0.2D1 - t1227 + t882 * t1229) -
     # 0.180D3 * t6 * t130 * (t882 * t1231 - t1229) - t6 * t140 * t1231)
     # * t209 * t149 / 0.720D3
      t1324 = FJET(XB1, XB2, s, 0.0D0, -t742, t736, 0.0D0, -t746, t1323)
      t1326 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t910, -t1
     #027, x4)
      t1328 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t910, -t1
     #027, x4)
      t1330 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t910, -t1
     #027, x4)
      t1338 = t1026 * t1328
      t1346 = t1054 * t1330 * t1050
      t1364 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (-t1026 * t1326 + 
     #t1071 * t1328 - t1074 * t1330 / 0.2D1) * t1050 + 0.180D3 * t60 * 0
     #.3141592653589793D1 * (-t1338 + t1071 * t1330) * t1050 + t54 * t13
     #46) * t147 * t209 / 0.1440D4 - (-0.90D2 * t6 * 0.3141592653589793D
     #1 * (-t1338 + t1038 * t1330) * t1050 - 0.180D3 * t60 * t1346) * t1
     #47 * t210 / 0.720D3
      t1365 = FJET(XB1, XB2, s, 0.0D0, -t1024, 0.0D0, t1019, 0.0D0, t136
     #4)
      t1367 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0
     #.10D1, x4)
      t1371 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0
     #.10D1, x4)
      t1375 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0
     #.10D1, x4)
      t1377 = t6 * t140 * t1375
      t1386 = rrqg2qgh82J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0
     #.10D1, x4)
      t1391 = t6 * t171 * t1375
      t1413 = z * t1367
      t1424 = z * t1371
      t1434 = -t1375 - z * t1375 * t117
      t1436 = t6 * t140 * t1434
      t1546 = rrqg2qgh82J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0
     #.10D1, x4)
      t1552 = ((-0.90D2 * t6 * 0.3141592653589793D1 * t1367 + 0.180D3 * 
     #t6 * t130 * t1371 - t1377) * t252 - 0.90D2 * t6 * 0.31415926535897
     #93D1 * t1375 * t262 + (-t6 * t140 * t1371 - 0.90D2 * t6 * 0.314159
     #2653589793D1 * t1386 - t1391 + 0.180D3 * t6 * t130 * t1367) * t276
     # + (-0.90D2 * t6 * 0.3141592653589793D1 * t1371 + 0.180D3 * t6 * t
     #130 * t1375) * t288) * t147 / 0.2880D4 + t88 * t1375 / 0.2880D4 - 
     #(0.90D2 * t6 * 0.3141592653589793D1 * (-t1367 - t123 * t1375 / 0.2
     #D1 + t121 * t1371 + (-t1413 + t103 * t1371 - t106 * t1375 / 0.2D1)
     # * t117) - 0.180D3 * t6 * t130 * (-t1371 + t121 * t1375 + (-t1424 
     #+ t103 * t1375) * t117) + t1436) * t147 * t149 / 0.1440D4 - (t6 * 
     #t140 * (-t1371 + t155 * t1375) + 0.90D2 * t6 * 0.3141592653589793D
     #1 * (t155 * t1367 + t163 * t1375 / 0.6D1 - t1386 - t160 * t1371 / 
     #0.2D1) - t1391 - 0.180D3 * t6 * t130 * (t155 * t1371 - t1367 - t16
     #0 * t1375 / 0.2D1)) * t149 / 0.1440D4 - (0.90D2 * t6 * 0.314159265
     #3589793D1 * ((-t1424 + t192 * t1375) * t117 + t198 * t1375 - t1371
     #) - 0.180D3 * t6 * t130 * t1434) * t147 * t210 / 0.720D3 + (0.90D2
     # * t6 * 0.3141592653589793D1 * (-t216 * t1371 + t218 * t1375 / 0.2
     #D1 + t1367) - 0.180D3 * t6 * t130 * (t1371 - t216 * t1375) + t1377
     #) * t209 * t149 / 0.720D3 + t46 * t1371 / 0.2880D4 + t56 * t1367 /
     # 0.2880D4 - (0.90D2 * t6 * 0.3141592653589793D1 * ((-t1413 + t301 
     #* t1371 - t304 * t1375 / 0.2D1) * t117 - t309 * t1375 / 0.2D1 + t2
     #95 * t1371 - t1367) - 0.180D3 * t6 * t130 * (t295 * t1375 + (-t142
     #4 + t301 * t1375) * t117 - t1371) + t1436) * t147 * t209 / 0.1440D
     #4 + (t6 * t140 * (-t331 * t1375 + t1371) + 0.90D2 * t6 * 0.3141592
     #653589793D1 * (t337 * t1371 / 0.2D1 - t340 * t1375 / 0.6D1 + t1386
     # - t331 * t1367) + t1391 - 0.180D3 * t6 * t130 * (-t331 * t1371 + 
     #t1367 + t337 * t1375 / 0.2D1)) * t209 / 0.1440D4 + t6 * 0.31415926
     #53589793D1 * t1546 / 0.32D2 + t65 * t1386 / 0.2880D4
      t1553 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t1552)
      t1555 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, t747, 0.10D1, 0.1
     #0D1, x4)
      t1558 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, t747, 0.10D1, 0.1
     #0D1, x4)
      t1560 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, t747, 0.10D1, 0.1
     #0D1, x4)
      t1572 = z * t1560
      t1584 = z * t1555 * t811 + t1555
      t1599 = rrqg2qgh83J4(s, XB1, XB2, z, lh, wd, nf, t747, 0.10D1, 0.1
     #0D1, x4)
      t1651 = -(0.90D2 * t6 * 0.3141592653589793D1 * (t793 * t1555 / 0.2
     #D1 + t1558 + (z * t1558 - t758 * t1560 + t762 * t1555 / 0.2D1) * t
     #739 * t785 - t791 * t1560) - 0.180D3 * t6 * t130 * ((t1572 - t758 
     #* t1555) * t739 * t785 - t791 * t1555 + t1560) + t6 * t140 * t1584
     #) * t147 * t149 / 0.1440D4 - (t6 * t140 * (-t824 * t1555 + t1560) 
     #+ 0.90D2 * t6 * 0.3141592653589793D1 * (t829 * t1560 / 0.2D1 - t83
     #2 * t1555 / 0.6D1 + t1599 - t824 * t1558) + t6 * t171 * t1555 - 0.
     #180D3 * t6 * t130 * (t1558 - t824 * t1560 + t829 * t1555 / 0.2D1))
     # * t149 / 0.1440D4 - (0.90D2 * t6 * 0.3141592653589793D1 * (t1560 
     #- t856 * t1555 + (t1572 - t863 * t1555) * t739 * t785) - 0.180D3 *
     # t6 * t130 * t1584) * t147 * t210 / 0.720D3 + (0.90D2 * t6 * 0.314
     #1592653589793D1 * (-t883 * t1555 / 0.2D1 - t1558 + t882 * t1560) -
     # 0.180D3 * t6 * t130 * (t882 * t1555 - t1560) - t6 * t140 * t1555)
     # * t209 * t149 / 0.720D3
      t1652 = FJET(XB1, XB2, s, t736, 0.0D0, 0.0D0, -t742, -t746, t1651)
      t1654 = t358 * t357 + t546 * t545 + t734 * t733 + t903 * t902 + t1
     #006 * t1005 + t1094 * t1093 + t1147 * t1146 + t1225 * t1224 + t132
     #4 * t1323 + t1365 * t1364 + t1553 * t1552 + t1652 * t1651
      t1655 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, t747, t910, 0.10D
     #1, x4)
      t1656 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, t747, t910, 0.10D
     #1, x4)
      t1668 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, t747, t910, 0.10D
     #1, x4)
      t1687 = -(0.90D2 * t6 * 0.3141592653589793D1 * (-t1655 + t1107 * t
     #1656) + 0.180D3 * t6 * t130 * t1656) * t147 * t210 / 0.720D3 + (0.
     #90D2 * t6 * 0.3141592653589793D1 * (t1668 + t1127 * t1656 / 0.2D1 
     #- t1125 * t1655) - 0.180D3 * t6 * t130 * (t1655 - t1125 * t1656) +
     # t6 * t140 * t1656) * t209 * t149 / 0.720D3
      t1688 = FJET(XB1, XB2, s, t736, -t1099, 0.0D0, t1098, t1101, t1687
     #)
      t1690 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t910, 0.1
     #0D1, x4)
      t1692 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t910, 0.1
     #0D1, x4)
      t1705 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t910, 0.1
     #0D1, x4)
      t1718 = t6 * t140 * t1690
      t1745 = rrqg2qgh81J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t910, 0.1
     #0D1, x4)
      t1765 = -(0.90D2 * t6 * 0.3141592653589793D1 * (-t972 * t1690 + t1
     #692) - 0.180D3 * t6 * t130 * t1690) * t147 * t210 / 0.720D3 + (0.9
     #0D2 * t6 * 0.3141592653589793D1 * (t987 * t1692 - t1705 - t989 * t
     #1690 / 0.2D1) - 0.180D3 * t6 * t130 * (-t1692 + t987 * t1690) - t1
     #718) * t209 * t149 / 0.720D3 - (0.90D2 * t6 * 0.3141592653589793D1
     # * (-t915 * t1692 + t1705 + t916 * t1690 / 0.2D1) - 0.180D3 * t6 *
     # t130 * (t1692 - t915 * t1690) + t1718) * t147 * t209 / 0.1440D4 +
     # (t6 * t140 * (t939 * t1690 - t1692) + 0.90D2 * t6 * 0.31415926535
     #89793D1 * (t945 * t1690 / 0.6D1 - t1745 + t939 * t1705 - t944 * t1
     #692 / 0.2D1) - t6 * t171 * t1690 - 0.180D3 * t6 * t130 * (-t944 * 
     #t1690 / 0.2D1 - t1705 + t939 * t1692)) * t209 / 0.1440D4
      t1766 = FJET(XB1, XB2, s, t906, 0.0D0, -t909, 0.0D0, 0.0D0, t1765)
      t1768 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t910, -t1
     #027, x4)
      t1769 = t1026 * t1768
      t1770 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t910, -t1
     #027, x4)
      t1778 = t1054 * t1770 * t1050
      t1785 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t910, -t1
     #027, x4)
      t1806 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (-t1769 + t1038 * 
     #t1770) * t1050 - 0.180D3 * t60 * t1778) * t147 * t210 / 0.720D3 - 
     #(-0.90D2 * t6 * 0.3141592653589793D1 * (-t1026 * t1785 + t1071 * t
     #1768 - t1074 * t1770 / 0.2D1) * t1050 + 0.180D3 * t60 * 0.31415926
     #53589793D1 * (-t1769 + t1071 * t1770) * t1050 + t54 * t1778) * t14
     #7 * t209 / 0.1440D4
      t1807 = FJET(XB1, XB2, s, t1019, 0.0D0, -t1024, 0.0D0, 0.0D0, t180
     #6)
      t1809 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, t747, t910, 0.10D
     #1, x4)
      t1810 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, t747, t910, 0.10D
     #1, x4)
      t1825 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, t747, t910, 0.10D
     #1, x4)
      t1841 = -(0.90D2 * t6 * 0.3141592653589793D1 * (-t1809 + t1107 * t
     #1810) + 0.180D3 * t6 * t130 * t1810) * t147 * t210 / 0.720D3 + (0.
     #90D2 * t6 * 0.3141592653589793D1 * (t1127 * t1810 / 0.2D1 - t1125 
     #* t1809 + t1825) - 0.180D3 * t6 * t130 * (t1809 - t1125 * t1810) +
     # t6 * t140 * t1810) * t209 * t149 / 0.720D3
      t1842 = FJET(XB1, XB2, s, t1098, 0.0D0, -t1099, t736, t1101, t1841
     #)
      t1845 = t736 * t1008 * t1017
      t1846 = t2 * t737
      t1848 = Sqrt(t768 * t1030)
      t1849 = t110 * t1848
      t1851 = 0.2D1 * t1849 * x2
      t1852 = t185 * x1
      t1853 = t185 * t738
      t1857 = t1846 * (t1851 - t1852 - x2 + t1008 + t1853 + 0.1D1 - x3 +
     # t185) * t740 * t1017
      t1861 = t97 * s * t1 * x1 * t1017
      t1867 = t1846 * x2 * (-0.1D1 + t1008 + x1 - t774 - t738 + t775 + 0
     #.2D1 * t1849) * t740 * t1017
      t1868 = x2 * x1
      t1870 = x2 * t93
      t1871 = -t776 + t779 + t781 - t782 - x2 - x3 + t738 + 0.2D1 * t186
     #8 - t1870 - z - t777 + t783 - t94 + t185 + t1025 + t1043 - t1044
      t1883 = t1868 * z
      t1895 = t1853 - 0.2D1 * t1849 * t1025 - 0.2D1 * t1849 * t1868 + 0.
     #2D1 * t1008 * t738 - t778 * t1868 - 0.2D1 * t94 * t1025 + t94 * t1
     #1 * x2 + 0.2D1 * t1849 * t1883 + t1851 - t1852 - 0.3D1 * t1883 - t
     #1008 * x1 + t94 * x2 + 0.2D1 * t1849 * z + t1868 * t11 + 0.2D1 * t
     #1870 * z - t1870 * t11
      t1897 = 0.1D1 / (t1871 + t1895)
      t1898 = z - t1025 + t1883 + x2 - t1868
      t1899 = t1897 * t1898
      t1900 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, t747, t910, -t102
     #7, x4)
      t1906 = log(0.4D1 * t185 * t820 * t821 * t1033)
      t1907 = t1906 * t1897
      t1908 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, t747, t910, -t102
     #7, x4)
      t1916 = t6 * t130
      t1921 = -0.90D2 * t6 * 0.3141592653589793D1 * (t1899 * t1900 - t19
     #07 * t1898 * t1908) * t739 + 0.180D3 * t1916 * t1899 * t1908 * t73
     #9
      t1925 = FJET(XB1, XB2, s, t1845, -t1857, -t1861, t1867, t1101, -t1
     #921 * t147 * t210 / 0.720D3)
      t1928 = t147 * t209 * t149
      t1931 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, t747, t910, -t102
     #7, x4)
      t1933 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, t747, t910, -t102
     #7, x4)
      t1945 = -0.90D2 * t6 * 0.3141592653589793D1 * (t1899 * t1931 - t19
     #07 * t1898 * t1933) * t739 + 0.180D3 * t1916 * t1899 * t1933 * t73
     #9
      t1949 = FJET(XB1, XB2, s, t1867, -t1861, -t1857, t1845, t1101, -t1
     #945 * t147 * t210 / 0.720D3)
      t1953 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t910, 0.1
     #0D1, x4)
      t1954 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t910, 0.1
     #0D1, x4)
      t1967 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t910, 0.1
     #0D1, x4)
      t1981 = t6 * t140 * t1954
      t2007 = rrqg2qgh82J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t910, 0.1
     #0D1, x4)
      t2028 = -(0.90D2 * t6 * 0.3141592653589793D1 * (t1953 - t972 * t19
     #54) - 0.180D3 * t6 * t130 * t1954) * t147 * t210 / 0.720D3 + (0.90
     #D2 * t6 * 0.3141592653589793D1 * (-t1967 + t987 * t1953 - t989 * t
     #1954 / 0.2D1) - 0.180D3 * t6 * t130 * (t987 * t1954 - t1953) - t19
     #81) * t209 * t149 / 0.720D3 - (0.90D2 * t6 * 0.3141592653589793D1 
     #* (t916 * t1954 / 0.2D1 - t915 * t1953 + t1967) - 0.180D3 * t6 * t
     #130 * (t1953 - t915 * t1954) + t1981) * t147 * t209 / 0.1440D4 + (
     #t6 * t140 * (t939 * t1954 - t1953) + 0.90D2 * t6 * 0.3141592653589
     #793D1 * (t939 * t1967 - t2007 + t945 * t1954 / 0.6D1 - t944 * t195
     #3 / 0.2D1) - t6 * t171 * t1954 - 0.180D3 * t6 * t130 * (t939 * t19
     #53 - t944 * t1954 / 0.2D1 - t1967)) * t209 / 0.1440D4
      t2029 = FJET(XB1, XB2, s, -t909, 0.0D0, t906, 0.0D0, 0.0D0, t2028)
      t2031 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, t747, 0.10D1, 0.1
     #0D1, x4)
      t2033 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, t747, 0.10D1, 0.1
     #0D1, x4)
      t2035 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, t747, 0.10D1, 0.1
     #0D1, x4)
      t2048 = z * t2033
      t2060 = z * t2035 * t811 + t2035
      t2074 = rrqg2qgh82J4(s, XB1, XB2, z, lh, wd, nf, t747, 0.10D1, 0.1
     #0D1, x4)
      t2127 = -(0.90D2 * t6 * 0.3141592653589793D1 * ((z * t2031 - t758 
     #* t2033 + t762 * t2035 / 0.2D1) * t739 * t785 + t2031 + t793 * t20
     #35 / 0.2D1 - t791 * t2033) - 0.180D3 * t6 * t130 * ((t2048 - t758 
     #* t2035) * t739 * t785 - t791 * t2035 + t2033) + t6 * t140 * t2060
     #) * t147 * t149 / 0.1440D4 - (t6 * t140 * (-t824 * t2035 + t2033) 
     #+ 0.90D2 * t6 * 0.3141592653589793D1 * (t829 * t2033 / 0.2D1 - t82
     #4 * t2031 + t2074 - t832 * t2035 / 0.6D1) + t6 * t171 * t2035 - 0.
     #180D3 * t6 * t130 * (t829 * t2035 / 0.2D1 + t2031 - t824 * t2033))
     # * t149 / 0.1440D4 - (0.90D2 * t6 * 0.3141592653589793D1 * (t2033 
     #- t856 * t2035 + (t2048 - t863 * t2035) * t739 * t785) - 0.180D3 *
     # t6 * t130 * t2060) * t147 * t210 / 0.720D3 + (0.90D2 * t6 * 0.314
     #1592653589793D1 * (t882 * t2033 - t883 * t2035 / 0.2D1 - t2031) - 
     #0.180D3 * t6 * t130 * (-t2033 + t882 * t2035) - t6 * t140 * t2035)
     # * t209 * t149 / 0.720D3
      t2128 = FJET(XB1, XB2, s, -t742, 0.0D0, 0.0D0, t736, -t746, t2127)
      t2130 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, t747, t910, 0.10D
     #1, x4)
      t2132 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, t747, t910, 0.10D
     #1, x4)
      t2143 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, t747, t910, 0.10D
     #1, x4)
      t2162 = -(0.90D2 * t6 * 0.3141592653589793D1 * (t1107 * t2130 - t2
     #132) + 0.180D3 * t6 * t130 * t2130) * t147 * t210 / 0.720D3 + (0.9
     #0D2 * t6 * 0.3141592653589793D1 * (t2143 - t1125 * t2132 + t1127 *
     # t2130 / 0.2D1) - 0.180D3 * t6 * t130 * (t2132 - t1125 * t2130) + 
     #t6 * t140 * t2130) * t209 * t149 / 0.720D3
      t2163 = FJET(XB1, XB2, s, -t1099, t736, t1098, 0.0D0, t1101, t2162
     #)
      t2165 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t910, -t1
     #027, x4)
      t2166 = t1026 * t2165
      t2167 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t910, -t1
     #027, x4)
      t2175 = t1054 * t2167 * t1050
      t2182 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t910, -t1
     #027, x4)
      t2203 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (-t2166 + t1038 * 
     #t2167) * t1050 - 0.180D3 * t60 * t2175) * t147 * t210 / 0.720D3 - 
     #(-0.90D2 * t6 * 0.3141592653589793D1 * (-t1026 * t2182 + t1071 * t
     #2165 - t1074 * t2167 / 0.2D1) * t1050 + 0.180D3 * t60 * 0.31415926
     #53589793D1 * (-t2166 + t1071 * t2167) * t1050 + t54 * t2175) * t14
     #7 * t209 / 0.1440D4
      t2204 = FJET(XB1, XB2, s, -t1024, 0.0D0, t1019, 0.0D0, 0.0D0, t220
     #3)
      t2206 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, t747, t910, -t102
     #7, x4)
      t2208 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, t747, t910, -t102
     #7, x4)
      t2220 = -0.90D2 * t6 * 0.3141592653589793D1 * (t1899 * t2206 - t19
     #07 * t1898 * t2208) * t739 + 0.180D3 * t1916 * t1899 * t2208 * t73
     #9
      t2224 = FJET(XB1, XB2, s, -t1861, t1867, t1845, -t1857, t1101, -t2
     #220 * t147 * t210 / 0.720D3)
      t2228 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, t747, t910, -t102
     #7, x4)
      t2230 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, t747, t910, -t102
     #7, x4)
      t2242 = -0.90D2 * t6 * 0.3141592653589793D1 * (t1899 * t2228 - t19
     #07 * t1898 * t2230) * t739 + 0.180D3 * t1916 * t1899 * t2230 * t73
     #9
      t2246 = FJET(XB1, XB2, s, -t1857, t1845, t1867, -t1861, t1101, -t2
     #242 * t147 * t210 / 0.720D3)
      t2250 = t1688 * t1687 + t1766 * t1765 + t1807 * t1806 + t1842 * t1
     #841 - t1925 * t1921 * t1928 / 0.720D3 - t1949 * t1945 * t1928 / 0.
     #720D3 + t2029 * t2028 + t2128 * t2127 + t2163 * t2162 + t2204 * t2
     #203 - t2224 * t2220 * t1928 / 0.720D3 - t2246 * t2242 * t1928 / 0.
     #720D3
      rrqg2qght8s1e1 = t1654 + t2250

      end function



      doubleprecision function rrqg2qght8s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.10
     #D1, x4)
      t9 = x4 * 0.3141592653589793D1
      t10 = Sin(t9)
      t11 = t10 ** 2
      t12 = x3 * t11
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t14 * t16
      t18 = -0.1D1 + x3
      t19 = 0.1D1 / t18
      t20 = t17 * t19
      t23 = log(-0.4D1 * t12 * t20)
      t24 = t23 ** 2
      t26 = cos(t9)
      t28 = Sqrt(-x3 * t18)
      t33 = 0.1D1 / (-x3 - z + 0.2D1 * t26 * t28 * z)
      t37 = log(0.4D1 * t12 * t17)
      t38 = t37 ** 2
      t40 = -t24 * z * t33 / 0.2D1 - t38 / 0.2D1
      t44 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t48 = 0.3141592653589793D1 * lh
      t51 = 0.180D3 * t6 * t48 * t7
      t55 = t23 * z * t33 + t37
      t57 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t64 = lh ** 2
      t66 = 0.3141592653589793D1 ** 2
      t68 = 0.180D3 * t64 - 0.30D2 * t66
      t69 = t68 * 0.3141592653589793D1
      t71 = t6 * t69 * t7
      t74 = -z * t33 - 0.1D1
      t77 = 0.1D1 / x3
      t80 = t14 * t11
      t81 = t80 * t16
      t83 = log(0.4D1 * t81)
      t84 = t83 * t3
      t85 = t5 * lh
      t88 = t83 ** 2
      t89 = t88 * t3
      t94 = (0.180D3 * t84 * t85 + 0.45D2 * t89 * t5 + t6 * t68) * 0.314
     #1592653589793D1
      t97 = rrqg2qgh83J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t116 = (-0.90D2 * t89 * t85 + t6 * (0.60D2 * lh * t66 - 0.28849365
     #67583026D3 - 0.120D3 * t64 * lh) - 0.15D2 * t88 * t83 * t3 * t5 - 
     #t84 * t5 * t68) * 0.3141592653589793D1
      t119 = t6 * lh
      t124 = (-0.180D3 * t119 - 0.90D2 * t84 * t5) * 0.3141592653589793D
     #1
      t127 = z * t44
      t128 = x2 ** 2
      t129 = x3 * t128
      t130 = t129 * t11
      t133 = log(-0.4D1 * t130 * t20)
      t134 = t133 * z
      t140 = log(0.4D1 * t129 * t81)
      t148 = -z * t7 * t33 - t7
      t151 = 0.180D3 * t6 * t48 * t148
      t154 = 0.1D1 / x2
      t157 = t128 * t11
      t160 = log(0.4D1 * t157 * t17)
      t162 = t160 ** 2
      t177 = x1 ** 2
      t178 = x3 * t177
      t179 = t178 * t11
      t182 = log(-0.4D1 * t179 * t20)
      t183 = t182 * z
      t189 = log(0.4D1 * t178 * t81)
      t197 = 0.1D1 / x1
      t200 = t6 * 0.3141592653589793D1
      t202 = t154 * t197
      t206 = t128 * t177
      t209 = log(0.4D1 * t206 * t81)
      t219 = t177 * t11
      t222 = log(0.4D1 * t219 * t17)
      t223 = t222 ** 2
      t239 = (-0.90D2 * t6 * 0.3141592653589793D1 * t7 * t40 + (-0.90D2 
     #* t6 * 0.3141592653589793D1 * t44 + t51) * t55 + (-0.90D2 * t6 * 0
     #.3141592653589793D1 * t57 + 0.180D3 * t6 * t48 * t44 - t71) * t74)
     # * t77 / 0.2880D4 + t94 * t44 / 0.2880D4 + t6 * 0.3141592653589793
     #D1 * t97 / 0.32D2 + t116 * t7 / 0.2880D4 + t124 * t57 / 0.2880D4 -
     # (0.90D2 * t6 * 0.3141592653589793D1 * ((-t127 + t134 * t7) * t33 
     #- t44 + t140 * t7) - t151) * t77 * t154 / 0.1440D4 + (0.90D2 * t6 
     #* 0.3141592653589793D1 * (t57 - t160 * t44 + t162 * t7 / 0.2D1) - 
     #0.180D3 * t6 * t48 * (t44 - t160 * t7) + t71) * t154 / 0.1440D4 - 
     #(0.90D2 * t6 * 0.3141592653589793D1 * ((-t127 + t183 * t7) * t33 -
     # t44 + t189 * t7) - t151) * t77 * t197 / 0.1440D4 - t200 * t148 * 
     #t77 * t202 / 0.8D1 + (0.90D2 * t6 * 0.3141592653589793D1 * (t44 - 
     #t209 * t7) - t51) * t154 * t197 / 0.720D3 - (0.90D2 * t6 * 0.31415
     #92653589793D1 * (-t57 - t223 * t7 / 0.2D1 + t222 * t44) - 0.180D3 
     #* t6 * t48 * (-t44 + t222 * t7) - t71) * t197 / 0.1440D4
      t240 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t239)
      t242 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t247 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t253 = 0.180D3 * t6 * t48 * t242
      t256 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t264 = t6 * t69 * t242
      t272 = rrqg2qgh81J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t280 = z * t247
      t291 = -t242 - z * t242 * t33
      t294 = 0.180D3 * t6 * t48 * t291
      t354 = (-0.90D2 * t6 * 0.3141592653589793D1 * t242 * t40 + (-0.90D
     #2 * t6 * 0.3141592653589793D1 * t247 + t253) * t55 + (-0.90D2 * t6
     # * 0.3141592653589793D1 * t256 + 0.180D3 * t6 * t48 * t247 - t264)
     # * t74) * t77 / 0.2880D4 + t94 * t247 / 0.2880D4 + t6 * 0.31415926
     #53589793D1 * t272 / 0.32D2 + t116 * t242 / 0.2880D4 + t124 * t256 
     #/ 0.2880D4 - (0.90D2 * t6 * 0.3141592653589793D1 * ((-t280 + t134 
     #* t242) * t33 + t140 * t242 - t247) - t294) * t77 * t154 / 0.1440D
     #4 + (0.90D2 * t6 * 0.3141592653589793D1 * (t256 - t160 * t247 + t1
     #62 * t242 / 0.2D1) - 0.180D3 * t6 * t48 * (-t160 * t242 + t247) + 
     #t264) * t154 / 0.1440D4 - (0.90D2 * t6 * 0.3141592653589793D1 * ((
     #-t280 + t183 * t242) * t33 + t189 * t242 - t247) - t294) * t77 * t
     #197 / 0.1440D4 - t200 * t291 * t77 * t202 / 0.8D1 + (0.90D2 * t6 *
     # 0.3141592653589793D1 * (t247 - t209 * t242) - t253) * t154 * t197
     # / 0.720D3 - (0.90D2 * t6 * 0.3141592653589793D1 * (t222 * t247 - 
     #t256 - t223 * t242 / 0.2D1) - 0.180D3 * t6 * t48 * (-t247 + t222 *
     # t242) - t264) * t197 / 0.1440D4
      t355 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t354)
      t357 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t362 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t368 = 0.180D3 * t6 * t48 * t357
      t371 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t379 = t6 * t69 * t357
      t387 = rrqg2qgh84J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t395 = z * t362
      t406 = -z * t357 * t33 - t357
      t409 = 0.180D3 * t6 * t48 * t406
      t469 = (-0.90D2 * t6 * 0.3141592653589793D1 * t357 * t40 + (-0.90D
     #2 * t6 * 0.3141592653589793D1 * t362 + t368) * t55 + (-0.90D2 * t6
     # * 0.3141592653589793D1 * t371 + 0.180D3 * t6 * t48 * t362 - t379)
     # * t74) * t77 / 0.2880D4 + t94 * t362 / 0.2880D4 + t6 * 0.31415926
     #53589793D1 * t387 / 0.32D2 + t116 * t357 / 0.2880D4 + t124 * t371 
     #/ 0.2880D4 - (0.90D2 * t6 * 0.3141592653589793D1 * ((-t395 + t134 
     #* t357) * t33 + t140 * t357 - t362) - t409) * t77 * t154 / 0.1440D
     #4 + (0.90D2 * t6 * 0.3141592653589793D1 * (-t160 * t362 + t371 + t
     #162 * t357 / 0.2D1) - 0.180D3 * t6 * t48 * (-t160 * t357 + t362) +
     # t379) * t154 / 0.1440D4 - (0.90D2 * t6 * 0.3141592653589793D1 * (
     #(-t395 + t183 * t357) * t33 + t189 * t357 - t362) - t409) * t77 * 
     #t197 / 0.1440D4 - t200 * t406 * t77 * t202 / 0.8D1 + (0.90D2 * t6 
     #* 0.3141592653589793D1 * (t362 - t209 * t357) - t368) * t154 * t19
     #7 / 0.720D3 - (0.90D2 * t6 * 0.3141592653589793D1 * (-t371 - t223 
     #* t357 / 0.2D1 + t222 * t362) - 0.180D3 * t6 * t48 * (t222 * t357 
     #- t362) - t379) * t197 / 0.1440D4
      t470 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t469)
      t472 = t2 * x1
      t473 = -0.1D1 + x1
      t474 = x1 * z
      t475 = 0.1D1 - x1 + t474
      t476 = 0.1D1 / t475
      t478 = t2 * t473 * t476
      t479 = s * t15
      t481 = x1 * t473 * t476
      t482 = t479 * t481
      t483 = -t473
      t484 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, t483, 0.10D1, 0.10
     #D1, x4)
      t487 = t16 * t476
      t488 = t473 ** 2
      t493 = log(-0.4D1 * t178 * t80 * t487 * t488 * t19)
      t494 = t493 * z
      t495 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, t483, 0.10D1, 0.10
     #D1, x4)
      t499 = x3 * t475
      t501 = Sqrt(-t499 * t18)
      t505 = x3 * x1
      t506 = t505 * z
      t507 = 0.3D1 * t506
      t508 = x1 * t13
      t509 = x3 * t13
      t510 = t509 * x1
      t512 = 0.2D1 * t178 * z
      t513 = t178 * t13
      t514 = 0.2D1 * t505
      t515 = -z + 0.2D1 * t26 * t501 * z + t474 - t507 - t508 + t510 + t
     #512 - t513 - x3 + t514 - t178
      t516 = 0.1D1 / t515
      t519 = t17 * t476 * t488
      t522 = log(0.4D1 * t179 * t519)
      t529 = t475 * t516
      t531 = z * t495 * t529 + t495
      t543 = t206 * t11
      t546 = log(0.4D1 * t543 * t519)
      t563 = log(0.4D1 * t219 * t14 * t487 * t488)
      t565 = t563 ** 2
      t568 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, t483, 0.10D1, 0.10
     #D1, x4)
      t583 = -(0.90D2 * t6 * 0.3141592653589793D1 * ((z * t484 - t494 * 
     #t495) * t475 * t516 - t522 * t495 + t484) - 0.180D3 * t6 * t48 * t
     #531) * t77 * t197 / 0.1440D4 - t200 * t531 * t77 * t202 / 0.8D1 + 
     #(0.90D2 * t6 * 0.3141592653589793D1 * (-t484 + t546 * t495) + 0.18
     #0D3 * t6 * t48 * t495) * t154 * t197 / 0.720D3 - (0.90D2 * t6 * 0.
     #3141592653589793D1 * (-t563 * t484 + t565 * t495 / 0.2D1 + t568) -
     # 0.180D3 * t6 * t48 * (-t563 * t495 + t484) + t6 * t69 * t495) * t
     #197 / 0.1440D4
      t584 = FJET(XB1, XB2, s, 0.0D0, t472, -t478, 0.0D0, -t482, t583)
      t586 = x2 * s
      t587 = t586 * t1
      t588 = -0.1D1 + x2
      t589 = t588 * s
      t590 = t589 * t1
      t591 = t17 * t588
      t594 = log(-0.4D1 * t130 * t591)
      t595 = -t588
      t596 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t595, 0.10
     #D1, x4)
      t598 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t595, 0.10
     #D1, x4)
      t605 = 0.180D3 * t6 * t48 * t596
      t610 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t595, 0.10
     #D1, x4)
      t613 = log(-0.4D1 * t157 * t591)
      t614 = t613 ** 2
      t638 = log(-0.4D1 * t543 * t591)
      t648 = -(0.90D2 * t6 * 0.3141592653589793D1 * (-t594 * t596 + t598
     #) - t605) * t77 * t154 / 0.1440D4 + (0.90D2 * t6 * 0.3141592653589
     #793D1 * (-t610 - t614 * t596 / 0.2D1 + t613 * t598) - 0.180D3 * t6
     # * t48 * (-t598 + t613 * t596) - t6 * t69 * t596) * t154 / 0.1440D
     #4 - t200 * t596 * t77 * t202 / 0.8D1 + (0.90D2 * t6 * 0.3141592653
     #589793D1 * (t638 * t596 - t598) + t605) * t154 * t197 / 0.720D3
      t649 = FJET(XB1, XB2, s, 0.0D0, t587, 0.0D0, -t590, 0.0D0, t648)
      t651 = x2 * x3
      t654 = Sqrt(x3 * t588 * t18)
      t655 = t26 * t654
      t657 = 0.2D1 * t655 * x2
      t659 = 0.1D1 - x3 + t651
      t660 = 0.1D1 / t659
      t662 = t2 * (0.1D1 - x3 - x2 + t651 + t129 + t657) * t660
      t667 = t2 * x2 * (-0.1D1 + t651 + 0.2D1 * t655) * t660
      t668 = x2 * z
      t669 = t668 - z - x2
      t670 = t18 * t660
      t671 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t595, -t67
     #0, x4)
      t675 = t659 ** 2
      t681 = log(0.4D1 * t129 * t80 * t16 * t588 * t18 / t675)
      t682 = t681 * t669
      t683 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t595, -t67
     #0, x4)
      t687 = t651 * z
      t688 = t129 * z
      t694 = 0.1D1 / (-t129 + x3 - t668 + z + x2 - t687 + t688 - t657 - 
     #0.2D1 * t655 * z + 0.2D1 * t655 * t668)
      t698 = 0.3141592653589793D1 * t669
      t699 = t683 * t694
      t707 = t6 * t698
      t709 = t77 * t154 * t197
      t713 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (-t669 * t671 + t68
     #2 * t683) * t694 - 0.180D3 * t119 * t698 * t699) * t77 * t154 / 0.
     #1440D4 - t707 * t699 * t709 / 0.8D1
      t714 = FJET(XB1, XB2, s, 0.0D0, t662, 0.0D0, -t667, 0.0D0, t713)
      t716 = t1 * t473
      t718 = t589 * t716 * t476
      t719 = t586 * t716
      t721 = t479 * t588 * t481
      t722 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, t483, t595, 0.10D1
     #, x4)
      t727 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, t483, t595, 0.10D1
     #, x4)
      t733 = log(-0.4D1 * t206 * t80 * t487 * t488 * t588)
      t746 = t200 * t722 * t77 * t202 / 0.8D1 + (0.90D2 * t6 * 0.3141592
     #653589793D1 * (t727 - t733 * t722) - 0.180D3 * t6 * t48 * t722) * 
     #t154 * t197 / 0.720D3
      t747 = FJET(XB1, XB2, s, 0.0D0, t718, t472, -t719, t721, t746)
      t749 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t595, 0.10
     #D1, x4)
      t750 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t595, 0.10
     #D1, x4)
      t758 = 0.180D3 * t6 * t48 * t750
      t763 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t595, 0.10
     #D1, x4)
      t794 = -(0.90D2 * t6 * 0.3141592653589793D1 * (t749 - t594 * t750)
     # - t758) * t77 * t154 / 0.1440D4 + (0.90D2 * t6 * 0.31415926535897
     #93D1 * (-t763 - t614 * t750 / 0.2D1 + t613 * t749) - 0.180D3 * t6 
     #* t48 * (-t749 + t613 * t750) - t6 * t69 * t750) * t154 / 0.1440D4
     # - t200 * t750 * t77 * t202 / 0.8D1 + (0.90D2 * t6 * 0.31415926535
     #89793D1 * (-t749 + t638 * t750) + t758) * t154 * t197 / 0.720D3
      t795 = FJET(XB1, XB2, s, 0.0D0, -t590, 0.0D0, t587, 0.0D0, t794)
      t797 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, t483, 0.10D1, 0.10
     #D1, x4)
      t799 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, t483, 0.10D1, 0.10
     #D1, x4)
      t811 = z * t799 * t529 + t799
      t835 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, t483, 0.10D1, 0.10
     #D1, x4)
      t853 = -(0.90D2 * t6 * 0.3141592653589793D1 * ((z * t797 - t494 * 
     #t799) * t475 * t516 - t522 * t799 + t797) - 0.180D3 * t6 * t48 * t
     #811) * t77 * t197 / 0.1440D4 - t200 * t811 * t77 * t202 / 0.8D1 + 
     #(0.90D2 * t6 * 0.3141592653589793D1 * (t546 * t799 - t797) + 0.180
     #D3 * t6 * t48 * t799) * t154 * t197 / 0.720D3 - (0.90D2 * t6 * 0.3
     #141592653589793D1 * (t835 - t563 * t797 + t565 * t799 / 0.2D1) - 0
     #.180D3 * t6 * t48 * (t797 - t563 * t799) + t6 * t69 * t799) * t197
     # / 0.1440D4
      t854 = FJET(XB1, XB2, s, 0.0D0, -t478, t472, 0.0D0, -t482, t853)
      t856 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t595, -t67
     #0, x4)
      t858 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t595, -t67
     #0, x4)
      t865 = t858 * t694
      t876 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (-t669 * t856 + t68
     #2 * t858) * t694 - 0.180D3 * t119 * t698 * t865) * t77 * t154 / 0.
     #1440D4 - t707 * t865 * t709 / 0.8D1
      t877 = FJET(XB1, XB2, s, 0.0D0, -t667, 0.0D0, t662, 0.0D0, t876)
      t879 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t884 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t890 = 0.180D3 * t6 * t48 * t879
      t893 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t901 = t6 * t69 * t879
      t907 = rrqg2qgh82J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t916 = z * t884
      t926 = -t879 - z * t879 * t33
      t929 = 0.180D3 * t6 * t48 * t926
      t991 = (-0.90D2 * t6 * 0.3141592653589793D1 * t879 * t40 + (-0.90D
     #2 * t6 * 0.3141592653589793D1 * t884 + t890) * t55 + (-0.90D2 * t6
     # * 0.3141592653589793D1 * t893 + 0.180D3 * t6 * t48 * t884 - t901)
     # * t74) * t77 / 0.2880D4 + t6 * 0.3141592653589793D1 * t907 / 0.32
     #D2 + t124 * t893 / 0.2880D4 + t94 * t884 / 0.2880D4 - (0.90D2 * t6
     # * 0.3141592653589793D1 * (t140 * t879 + (-t916 + t134 * t879) * t
     #33 - t884) - t929) * t77 * t154 / 0.1440D4 + (0.90D2 * t6 * 0.3141
     #592653589793D1 * (-t160 * t884 + t893 + t162 * t879 / 0.2D1) - 0.1
     #80D3 * t6 * t48 * (-t160 * t879 + t884) + t901) * t154 / 0.1440D4 
     #+ t116 * t879 / 0.2880D4 - (0.90D2 * t6 * 0.3141592653589793D1 * (
     #-t884 + t189 * t879 + (-t916 + t183 * t879) * t33) - t929) * t77 *
     # t197 / 0.1440D4 - t200 * t926 * t77 * t202 / 0.8D1 + (0.90D2 * t6
     # * 0.3141592653589793D1 * (t884 - t209 * t879) - t890) * t154 * t1
     #97 / 0.720D3 - (0.90D2 * t6 * 0.3141592653589793D1 * (t222 * t884 
     #- t893 - t223 * t879 / 0.2D1) - 0.180D3 * t6 * t48 * (-t884 + t222
     # * t879) - t901) * t197 / 0.1440D4
      t992 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t991)
      t994 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, t483, 0.10D1, 0.10
     #D1, x4)
      t996 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, t483, 0.10D1, 0.10
     #D1, x4)
      t1008 = z * t996 * t529 + t996
      t1032 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, t483, 0.10D1, 0.1
     #0D1, x4)
      t1050 = -(0.90D2 * t6 * 0.3141592653589793D1 * ((z * t994 - t494 *
     # t996) * t475 * t516 - t522 * t996 + t994) - 0.180D3 * t6 * t48 * 
     #t1008) * t77 * t197 / 0.1440D4 - t200 * t1008 * t77 * t202 / 0.8D1
     # + (0.90D2 * t6 * 0.3141592653589793D1 * (t546 * t996 - t994) + 0.
     #180D3 * t6 * t48 * t996) * t154 * t197 / 0.720D3 - (0.90D2 * t6 * 
     #0.3141592653589793D1 * (t1032 - t563 * t994 + t565 * t996 / 0.2D1)
     # - 0.180D3 * t6 * t48 * (-t563 * t996 + t994) + t6 * t69 * t996) *
     # t197 / 0.1440D4
      t1051 = FJET(XB1, XB2, s, t472, 0.0D0, 0.0D0, -t478, -t482, t1050)
      t1053 = t240 * t239 + t355 * t354 + t470 * t469 + t584 * t583 + t6
     #49 * t648 + t714 * t713 + t747 * t746 + t795 * t794 + t854 * t853 
     #+ t877 * t876 + t992 * t991 + t1051 * t1050
      t1054 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, t483, t595, 0.10D
     #1, x4)
      t1059 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, t483, t595, 0.10D
     #1, x4)
      t1072 = t200 * t1054 * t77 * t202 / 0.8D1 + (0.90D2 * t6 * 0.31415
     #92653589793D1 * (t1059 - t733 * t1054) - 0.180D3 * t6 * t48 * t105
     #4) * t154 * t197 / 0.720D3
      t1073 = FJET(XB1, XB2, s, t472, -t719, 0.0D0, t718, t721, t1072)
      t1075 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t595, 0.1
     #0D1, x4)
      t1076 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t595, 0.1
     #0D1, x4)
      t1084 = 0.180D3 * t6 * t48 * t1076
      t1091 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t595, 0.1
     #0D1, x4)
      t1120 = -(0.90D2 * t6 * 0.3141592653589793D1 * (t1075 - t594 * t10
     #76) - t1084) * t77 * t154 / 0.1440D4 + (0.90D2 * t6 * 0.3141592653
     #589793D1 * (-t614 * t1076 / 0.2D1 - t1091 + t613 * t1075) - 0.180D
     #3 * t6 * t48 * (t613 * t1076 - t1075) - t6 * t69 * t1076) * t154 /
     # 0.1440D4 - t200 * t1076 * t77 * t202 / 0.8D1 + (0.90D2 * t6 * 0.3
     #141592653589793D1 * (-t1075 + t638 * t1076) + t1084) * t154 * t197
     # / 0.720D3
      t1121 = FJET(XB1, XB2, s, t587, 0.0D0, -t590, 0.0D0, 0.0D0, t1120)
      t1123 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t595, -t6
     #70, x4)
      t1125 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t595, -t6
     #70, x4)
      t1132 = t1125 * t694
      t1143 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (-t669 * t1123 + t
     #682 * t1125) * t694 - 0.180D3 * t119 * t698 * t1132) * t77 * t154 
     #/ 0.1440D4 - t707 * t1132 * t709 / 0.8D1
      t1144 = FJET(XB1, XB2, s, t662, 0.0D0, -t667, 0.0D0, 0.0D0, t1143)
      t1146 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, t483, t595, 0.10D
     #1, x4)
      t1151 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, t483, t595, 0.10D
     #1, x4)
      t1164 = t200 * t1146 * t77 * t202 / 0.8D1 + (0.90D2 * t6 * 0.31415
     #92653589793D1 * (t1151 - t733 * t1146) - 0.180D3 * t6 * t48 * t114
     #6) * t154 * t197 / 0.720D3
      t1165 = FJET(XB1, XB2, s, t718, 0.0D0, -t719, t472, t721, t1164)
      t1168 = t472 * t651 * t660
      t1169 = t2 * t473
      t1172 = Sqrt(t499 * t588 * t18)
      t1173 = t26 * t1172
      t1175 = 0.2D1 * t1173 * x2
      t1176 = t129 * x1
      t1177 = t129 * t474
      t1181 = t1169 * (t1175 - t1176 - x2 + t651 + t1177 + 0.1D1 - x3 + 
     #t129) * t476 * t660
      t1185 = t18 * s * t1 * x1 * t660
      t1191 = t1169 * x2 * (-0.1D1 + t651 + x1 - t505 - t474 + t506 + 0.
     #2D1 * t1173) * t476 * t660
      t1192 = x2 * x1
      t1194 = x2 * t177
      t1195 = t1192 * z
      t1201 = 0.2D1 * t1192 - t1194 + t474 - t508 + t514 - t178 + t129 +
     # t668 - z - x3 - x2 + t1175 - t1176 - 0.3D1 * t1195 - t651 * x1 + 
     #t178 * x2 + 0.2D1 * t1173 * z
      t1219 = t1192 * t13 + 0.2D1 * t1194 * z - t1194 * t13 + 0.2D1 * t1
     #173 * t1195 + t1177 - 0.2D1 * t1173 * t668 - 0.2D1 * t1173 * t1192
     # + 0.2D1 * t651 * t474 - t509 * t1192 - 0.2D1 * t178 * t668 + t178
     # * t13 * x2 + t687 - t688 - t507 + t510 + t512 - t513
      t1221 = 0.1D1 / (t1201 + t1219)
      t1223 = z - t668 + t1195 + x2 - t1192
      t1225 = t6 * 0.3141592653589793D1 * t1221 * t1223
      t1226 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, t483, t595, -t670
     #, x4)
      t1231 = FJET(XB1, XB2, s, t1168, -t1181, -t1185, t1191, t721, t122
     #5 * t1226 * t475 * t709 / 0.8D1)
      t1234 = t5 * 0.3141592653589793D1 * t1221
      t1241 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, t483, t595, -t670
     #, x4)
      t1246 = FJET(XB1, XB2, s, t1191, -t1185, -t1181, t1168, t721, t122
     #5 * t1241 * t475 * t709 / 0.8D1)
      t1254 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t595, 0.1
     #0D1, x4)
      t1255 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t595, 0.1
     #0D1, x4)
      t1263 = 0.180D3 * t6 * t48 * t1255
      t1271 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t595, 0.1
     #0D1, x4)
      t1299 = -(0.90D2 * t6 * 0.3141592653589793D1 * (t1254 - t594 * t12
     #55) - t1263) * t77 * t154 / 0.1440D4 + (0.90D2 * t6 * 0.3141592653
     #589793D1 * (t613 * t1254 - t614 * t1255 / 0.2D1 - t1271) - 0.180D3
     # * t6 * t48 * (t613 * t1255 - t1254) - t6 * t69 * t1255) * t154 / 
     #0.1440D4 - t200 * t1255 * t77 * t202 / 0.8D1 + (0.90D2 * t6 * 0.31
     #41592653589793D1 * (t638 * t1255 - t1254) + t1263) * t154 * t197 /
     # 0.720D3
      t1300 = FJET(XB1, XB2, s, -t590, 0.0D0, t587, 0.0D0, 0.0D0, t1299)
      t1302 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, t483, 0.10D1, 0.1
     #0D1, x4)
      t1304 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, t483, 0.10D1, 0.1
     #0D1, x4)
      t1316 = z * t1304 * t529 + t1304
      t1342 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, t483, 0.10D1, 0.1
     #0D1, x4)
      t1358 = -(0.90D2 * t6 * 0.3141592653589793D1 * ((z * t1302 - t494 
     #* t1304) * t475 * t516 - t522 * t1304 + t1302) - 0.180D3 * t6 * t4
     #8 * t1316) * t77 * t197 / 0.1440D4 - t200 * t1316 * t77 * t202 / 0
     #.8D1 + (0.90D2 * t6 * 0.3141592653589793D1 * (-t1302 + t546 * t130
     #4) + 0.180D3 * t6 * t48 * t1304) * t154 * t197 / 0.720D3 - (0.90D2
     # * t6 * 0.3141592653589793D1 * (t565 * t1304 / 0.2D1 + t1342 - t56
     #3 * t1302) - 0.180D3 * t6 * t48 * (-t563 * t1304 + t1302) + t6 * t
     #69 * t1304) * t197 / 0.1440D4
      t1359 = FJET(XB1, XB2, s, -t478, 0.0D0, 0.0D0, t472, -t482, t1358)
      t1361 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, t483, t595, 0.10D
     #1, x4)
      t1366 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, t483, t595, 0.10D
     #1, x4)
      t1379 = t200 * t1361 * t77 * t202 / 0.8D1 + (0.90D2 * t6 * 0.31415
     #92653589793D1 * (t1366 - t733 * t1361) - 0.180D3 * t6 * t48 * t136
     #1) * t154 * t197 / 0.720D3
      t1380 = FJET(XB1, XB2, s, -t719, t472, t718, 0.0D0, t721, t1379)
      t1382 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t595, -t6
     #70, x4)
      t1384 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t595, -t6
     #70, x4)
      t1391 = t1384 * t694
      t1402 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (-t669 * t1382 + t
     #682 * t1384) * t694 - 0.180D3 * t119 * t698 * t1391) * t77 * t154 
     #/ 0.1440D4 - t707 * t1391 * t709 / 0.8D1
      t1403 = FJET(XB1, XB2, s, -t667, 0.0D0, t662, 0.0D0, 0.0D0, t1402)
      t1405 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, t483, t595, -t670
     #, x4)
      t1410 = FJET(XB1, XB2, s, -t1185, t1191, t1168, -t1181, t721, t122
     #5 * t1405 * t475 * t709 / 0.8D1)
      t1418 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, t483, t595, -t670
     #, x4)
      t1423 = FJET(XB1, XB2, s, -t1181, t1168, t1191, -t1185, t721, t122
     #5 * t1418 * t475 * t709 / 0.8D1)
      t1431 = t1073 * t1072 + t1121 * t1120 + t1144 * t1143 + t1165 * t1
     #164 + t1231 * t3 * t1234 * t1223 * t1226 * t475 * t709 / 0.8D1 + t
     #1246 * t3 * t1234 * t1223 * t1241 * t475 * t709 / 0.8D1 + t1300 * 
     #t1299 + t1359 * t1358 + t1380 * t1379 + t1403 * t1402 + t1410 * t3
     # * t1234 * t1223 * t1405 * t475 * t709 / 0.8D1 + t1423 * t3 * t123
     #4 * t1223 * t1418 * t475 * t709 / 0.8D1
      rrqg2qght8s1e0 = t1053 + t1431

      end function



      doubleprecision function rrqg2qght8s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.10
     #D1, x4)
      t9 = x4 * 0.3141592653589793D1
      t10 = Sin(t9)
      t11 = t10 ** 2
      t12 = x3 * t11
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t14 * t16
      t18 = -0.1D1 + x3
      t23 = log(-0.4D1 * t12 * t17 / t18)
      t25 = cos(t9)
      t27 = Sqrt(-x3 * t18)
      t32 = 0.1D1 / (-x3 - z + 0.2D1 * t25 * t27 * z)
      t36 = log(0.4D1 * t12 * t17)
      t37 = t23 * z * t32 + t36
      t41 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t45 = 0.3141592653589793D1 * lh
      t48 = 0.180D3 * t6 * t45 * t7
      t51 = -z * t32 - 0.1D1
      t54 = 0.1D1 / x3
      t57 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t66 = log(0.4D1 * t14 * t11 * t16)
      t67 = t66 * t3
      t71 = (-0.180D3 * t6 * lh - 0.90D2 * t67 * t5) * 0.314159265358979
     #3D1
      t77 = t66 ** 2
      t81 = lh ** 2
      t83 = 0.3141592653589793D1 ** 2
      t88 = (0.180D3 * t67 * t5 * lh + 0.45D2 * t77 * t3 * t5 + t6 * (0.
     #180D3 * t81 - 0.30D2 * t83)) * 0.3141592653589793D1
      t91 = t6 * 0.3141592653589793D1
      t95 = (-z * t7 * t32 - t7) * t54
      t96 = 0.1D1 / x2
      t100 = x2 ** 2
      t101 = t100 * t11
      t104 = log(0.4D1 * t101 * t17)
      t114 = 0.1D1 / x1
      t118 = x1 ** 2
      t119 = t118 * t11
      t122 = log(0.4D1 * t119 * t17)
      t134 = (-0.90D2 * t6 * 0.3141592653589793D1 * t7 * t37 + (-0.90D2 
     #* t6 * 0.3141592653589793D1 * t41 + t48) * t51) * t54 / 0.2880D4 +
     # t6 * 0.3141592653589793D1 * t57 / 0.32D2 + t71 * t41 / 0.2880D4 +
     # t88 * t7 / 0.2880D4 - t91 * t95 * t96 / 0.16D2 + (0.90D2 * t6 * 0
     #.3141592653589793D1 * (t41 - t104 * t7) - t48) * t96 / 0.1440D4 + 
     #t91 * t7 * t96 * t114 / 0.8D1 - (0.90D2 * t6 * 0.3141592653589793D
     #1 * (-t41 + t122 * t7) + t48) * t114 / 0.1440D4 - t91 * t95 * t114
     # / 0.16D2
      t135 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t134)
      t137 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t142 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t148 = 0.180D3 * t6 * t45 * t137
      t154 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t165 = (-t137 - z * t137 * t32) * t54
      t192 = (-0.90D2 * t6 * 0.3141592653589793D1 * t137 * t37 + (-0.90D
     #2 * t6 * 0.3141592653589793D1 * t142 + t148) * t51) * t54 / 0.2880
     #D4 + t6 * 0.3141592653589793D1 * t154 / 0.32D2 + t71 * t142 / 0.28
     #80D4 + t88 * t137 / 0.2880D4 - t91 * t165 * t96 / 0.16D2 + (0.90D2
     # * t6 * 0.3141592653589793D1 * (-t104 * t137 + t142) - t148) * t96
     # / 0.1440D4 + t91 * t137 * t96 * t114 / 0.8D1 - (0.90D2 * t6 * 0.3
     #141592653589793D1 * (-t142 + t122 * t137) + t148) * t114 / 0.1440D
     #4 - t91 * t165 * t114 / 0.16D2
      t193 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t192)
      t195 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t200 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t206 = 0.180D3 * t6 * t45 * t195
      t212 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t223 = (-z * t195 * t32 - t195) * t54
      t250 = (-0.90D2 * t6 * 0.3141592653589793D1 * t195 * t37 + (-0.90D
     #2 * t6 * 0.3141592653589793D1 * t200 + t206) * t51) * t54 / 0.2880
     #D4 + t6 * 0.3141592653589793D1 * t212 / 0.32D2 + t71 * t200 / 0.28
     #80D4 + t88 * t195 / 0.2880D4 - t91 * t223 * t96 / 0.16D2 + (0.90D2
     # * t6 * 0.3141592653589793D1 * (-t104 * t195 + t200) - t206) * t96
     # / 0.1440D4 + t91 * t195 * t96 * t114 / 0.8D1 - (0.90D2 * t6 * 0.3
     #141592653589793D1 * (t122 * t195 - t200) + t206) * t114 / 0.1440D4
     # - t91 * t223 * t114 / 0.16D2
      t251 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t250)
      t253 = t2 * x1
      t254 = -0.1D1 + x1
      t255 = x1 * z
      t256 = 0.1D1 - x1 + t255
      t257 = 0.1D1 / t256
      t259 = t2 * t254 * t257
      t260 = s * t15
      t262 = x1 * t254 * t257
      t263 = t260 * t262
      t264 = -t254
      t265 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, t264, 0.10D1, 0.10
     #D1, x4)
      t272 = t254 ** 2
      t276 = log(0.4D1 * t119 * t14 * t16 * t257 * t272)
      t278 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, t264, 0.10D1, 0.10
     #D1, x4)
      t292 = Sqrt(-x3 * t256 * t18)
      t296 = x3 * x1
      t302 = x3 * t118
      t307 = -z + 0.2D1 * t25 * t292 * z + t255 - 0.3D1 * t296 * z - x1 
     #* t13 + x3 * t13 * x1 + 0.2D1 * t302 * z - t302 * t13 - x3 + 0.2D1
     # * t296 - t302
      t309 = t256 / t307
      t316 = -t91 * t265 * t96 * t114 / 0.8D1 - (0.90D2 * t6 * 0.3141592
     #653589793D1 * (-t276 * t265 + t278) - 0.180D3 * t6 * t45 * t265) *
     # t114 / 0.1440D4 - t91 * (z * t265 * t309 + t265) * t54 * t114 / 0
     #.16D2
      t317 = FJET(XB1, XB2, s, 0.0D0, t253, -t259, 0.0D0, -t263, t316)
      t319 = x2 * s
      t320 = t319 * t1
      t321 = -0.1D1 + x2
      t322 = t321 * s
      t323 = t322 * t1
      t324 = -t321
      t325 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t324, 0.10
     #D1, x4)
      t330 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t324, 0.10
     #D1, x4)
      t334 = log(-0.4D1 * t101 * t17 * t321)
      t350 = -t91 * t325 * t54 * t96 / 0.16D2 + (0.90D2 * t6 * 0.3141592
     #653589793D1 * (-t330 + t334 * t325) + 0.180D3 * t6 * t45 * t325) *
     # t96 / 0.1440D4 - t91 * t325 * t96 * t114 / 0.8D1
      t351 = FJET(XB1, XB2, s, 0.0D0, t320, 0.0D0, -t323, 0.0D0, t350)
      t353 = x2 * x3
      t354 = t100 * x3
      t357 = Sqrt(x3 * t321 * t18)
      t358 = t25 * t357
      t360 = 0.2D1 * t358 * x2
      t363 = 0.1D1 / (0.1D1 - x3 + t353)
      t365 = t2 * (0.1D1 - x3 - x2 + t353 + t354 + t360) * t363
      t370 = t2 * x2 * (-0.1D1 + t353 + 0.2D1 * t358) * t363
      t371 = x2 * z
      t372 = t371 - z - x2
      t374 = t6 * 0.3141592653589793D1 * t372
      t375 = t18 * t363
      t376 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t324, -t37
     #5, x4)
      t384 = 0.1D1 / (-t354 + x3 - t371 + z + x2 - t353 * z + t354 * z -
     # t360 - 0.2D1 * t358 * z + 0.2D1 * t358 * t371)
      t386 = t54 * t96
      t390 = FJET(XB1, XB2, s, 0.0D0, t365, 0.0D0, -t370, 0.0D0, -t374 *
     # t376 * t384 * t386 / 0.16D2)
      t392 = t5 * 0.3141592653589793D1
      t396 = t384 * t54 * t96
      t400 = t1 * t254
      t402 = t322 * t400 * t257
      t403 = t319 * t400
      t405 = t260 * t321 * t262
      t406 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, t264, t324, 0.10D1
     #, x4)
      t411 = FJET(XB1, XB2, s, 0.0D0, t402, t253, -t403, t405, t91 * t40
     #6 * t96 * t114 / 0.8D1)
      t415 = t96 * t114
      t419 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t324, 0.10
     #D1, x4)
      t424 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t324, 0.10
     #D1, x4)
      t440 = -t91 * t419 * t54 * t96 / 0.16D2 + (0.90D2 * t6 * 0.3141592
     #653589793D1 * (-t424 + t334 * t419) + 0.180D3 * t6 * t45 * t419) *
     # t96 / 0.1440D4 - t91 * t419 * t96 * t114 / 0.8D1
      t441 = FJET(XB1, XB2, s, 0.0D0, -t323, 0.0D0, t320, 0.0D0, t440)
      t443 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, t264, 0.10D1, 0.10
     #D1, x4)
      t448 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, t264, 0.10D1, 0.10
     #D1, x4)
      t467 = -t91 * t443 * t96 * t114 / 0.8D1 - (0.90D2 * t6 * 0.3141592
     #653589793D1 * (t448 - t276 * t443) - 0.180D3 * t6 * t45 * t443) * 
     #t114 / 0.1440D4 - t91 * (z * t443 * t309 + t443) * t54 * t114 / 0.
     #16D2
      t468 = FJET(XB1, XB2, s, 0.0D0, -t259, t253, 0.0D0, -t263, t467)
      t470 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t324, -t37
     #5, x4)
      t475 = FJET(XB1, XB2, s, 0.0D0, -t370, 0.0D0, t365, 0.0D0, -t374 *
     # t470 * t384 * t386 / 0.16D2)
      t482 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t486 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t493 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t499 = 0.180D3 * t6 * t45 * t486
      t508 = (-t486 - z * t486 * t32) * t54
      t537 = t6 * 0.3141592653589793D1 * t482 / 0.32D2 + t88 * t486 / 0.
     #2880D4 + (-0.90D2 * t6 * 0.3141592653589793D1 * t486 * t37 + (-0.9
     #0D2 * t6 * 0.3141592653589793D1 * t493 + t499) * t51) * t54 / 0.28
     #80D4 - t91 * t508 * t96 / 0.16D2 + (0.90D2 * t6 * 0.31415926535897
     #93D1 * (-t104 * t486 + t493) - t499) * t96 / 0.1440D4 + t71 * t493
     # / 0.2880D4 + t91 * t486 * t96 * t114 / 0.8D1 - (0.90D2 * t6 * 0.3
     #141592653589793D1 * (-t493 + t122 * t486) + t499) * t114 / 0.1440D
     #4 - t91 * t508 * t114 / 0.16D2
      t538 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t537)
      t540 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, t264, 0.10D1, 0.10
     #D1, x4)
      t546 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, t264, 0.10D1, 0.10
     #D1, x4)
      t564 = -t91 * t540 * t96 * t114 / 0.8D1 - (0.90D2 * t6 * 0.3141592
     #653589793D1 * (-t276 * t540 + t546) - 0.180D3 * t6 * t45 * t540) *
     # t114 / 0.1440D4 - t91 * (z * t540 * t309 + t540) * t54 * t114 / 0
     #.16D2
      t565 = FJET(XB1, XB2, s, t253, 0.0D0, 0.0D0, -t259, -t263, t564)
      t567 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, t264, t324, 0.10D1
     #, x4)
      t572 = FJET(XB1, XB2, s, t253, -t403, 0.0D0, t402, t405, t91 * t56
     #7 * t96 * t114 / 0.8D1)
      t579 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t324, 0.10
     #D1, x4)
      t585 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t324, 0.10
     #D1, x4)
      t600 = -t91 * t579 * t54 * t96 / 0.16D2 + (0.90D2 * t6 * 0.3141592
     #653589793D1 * (t334 * t579 - t585) + 0.180D3 * t6 * t45 * t579) * 
     #t96 / 0.1440D4 - t91 * t579 * t96 * t114 / 0.8D1
      t601 = FJET(XB1, XB2, s, t320, 0.0D0, -t323, 0.0D0, 0.0D0, t600)
      t603 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t324, -t37
     #5, x4)
      t608 = FJET(XB1, XB2, s, t365, 0.0D0, -t370, 0.0D0, 0.0D0, -t374 *
     # t603 * t384 * t386 / 0.16D2)
      t615 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, t264, t324, 0.10D1
     #, x4)
      t620 = FJET(XB1, XB2, s, t402, 0.0D0, -t403, t253, t405, t91 * t61
     #5 * t96 * t114 / 0.8D1)
      t627 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t324, 0.10
     #D1, x4)
      t633 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t324, 0.10
     #D1, x4)
      t648 = -t91 * t627 * t54 * t96 / 0.16D2 + (0.90D2 * t6 * 0.3141592
     #653589793D1 * (t334 * t627 - t633) + 0.180D3 * t6 * t45 * t627) * 
     #t96 / 0.1440D4 - t91 * t627 * t96 * t114 / 0.8D1
      t649 = FJET(XB1, XB2, s, -t323, 0.0D0, t320, 0.0D0, 0.0D0, t648)
      t651 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, t264, 0.10D1, 0.10
     #D1, x4)
      t657 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, t264, 0.10D1, 0.10
     #D1, x4)
      t675 = -t91 * t651 * t96 * t114 / 0.8D1 - (0.90D2 * t6 * 0.3141592
     #653589793D1 * (-t276 * t651 + t657) - 0.180D3 * t6 * t45 * t651) *
     # t114 / 0.1440D4 - t91 * (z * t651 * t309 + t651) * t54 * t114 / 0
     #.16D2
      t676 = FJET(XB1, XB2, s, -t259, 0.0D0, 0.0D0, t253, -t263, t675)
      t678 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t324, -t37
     #5, x4)
      t683 = FJET(XB1, XB2, s, -t370, 0.0D0, t365, 0.0D0, 0.0D0, -t374 *
     # t678 * t384 * t386 / 0.16D2)
      t690 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, t264, t324, 0.10D1
     #, x4)
      t695 = FJET(XB1, XB2, s, -t403, t253, t402, 0.0D0, t405, t91 * t69
     #0 * t96 * t114 / 0.8D1)
      rrqg2qght8s1em1 = t135 * t134 + t193 * t192 + t251 * t250 + t317 *
     # t316 + t351 * t350 - t390 * t3 * t392 * t372 * t376 * t396 / 0.16
     #D2 + t411 * t3 * t5 * 0.3141592653589793D1 * t406 * t415 / 0.8D1 +
     # t441 * t440 + t468 * t467 - t475 * t3 * t392 * t372 * t470 * t396
     # / 0.16D2 + t538 * t537 + t565 * t564 + t572 * t3 * t5 * 0.3141592
     #653589793D1 * t567 * t415 / 0.8D1 + t601 * t600 - t608 * t3 * t392
     # * t372 * t603 * t396 / 0.16D2 + t620 * t3 * t5 * 0.31415926535897
     #93D1 * t615 * t415 / 0.8D1 + t649 * t648 + t676 * t675 - t683 * t3
     # * t392 * t372 * t678 * t396 / 0.16D2 + t695 * t3 * t5 * 0.3141592
     #653589793D1 * t690 * t415 / 0.8D1

      end function



      doubleprecision function rrqg2qght8s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = t6 * 0.3141592653589793D1
      t8 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.10
     #D1, x4)
      t9 = x4 * 0.3141592653589793D1
      t10 = cos(t9)
      t13 = Sqrt(-x3 * (-0.1D1 + x3))
      t20 = -z / (-x3 - z + 0.2D1 * t10 * t13 * z) - 0.1D1
      t22 = 0.1D1 / x3
      t26 = 0.3141592653589793D1 * t8
      t27 = 0.1D1 / x2
      t31 = 0.1D1 / x1
      t35 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t41 = z ** 2
      t43 = Sin(t9)
      t44 = t43 ** 2
      t46 = t1 ** 2
      t47 = t46 ** 2
      t50 = log(0.4D1 / t41 * t44 * t47)
      t55 = (-0.180D3 * t6 * lh - 0.90D2 * t50 * t3 * t5) * 0.3141592653
     #589793D1
      t58 = -t7 * t8 * t20 * t22 / 0.32D2 + t6 * t26 * t27 / 0.16D2 + t6
     # * t26 * t31 / 0.16D2 + t6 * 0.3141592653589793D1 * t35 / 0.32D2 +
     # t55 * t8 / 0.2880D4
      t59 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t58)
      t61 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t66 = 0.3141592653589793D1 * t61
      t73 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t79 = -t7 * t61 * t20 * t22 / 0.32D2 + t6 * t66 * t27 / 0.16D2 + t
     #6 * t66 * t31 / 0.16D2 + t6 * 0.3141592653589793D1 * t73 / 0.32D2 
     #+ t55 * t61 / 0.2880D4
      t80 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t79)
      t82 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t87 = 0.3141592653589793D1 * t82
      t94 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t100 = -t7 * t82 * t20 * t22 / 0.32D2 + t6 * t87 * t27 / 0.16D2 + 
     #t6 * t87 * t31 / 0.16D2 + t6 * 0.3141592653589793D1 * t94 / 0.32D2
     # + t55 * t82 / 0.2880D4
      t101 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t100)
      t103 = t2 * x1
      t104 = -0.1D1 + x1
      t107 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t109 = t2 * t104 * t107
      t113 = s * t46 * x1 * t104 * t107
      t114 = -t104
      t115 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, t114, 0.10D1, 0.10
     #D1, x4)
      t117 = 0.3141592653589793D1 * t115 * t31
      t120 = FJET(XB1, XB2, s, 0.0D0, t103, -t109, 0.0D0, -t113, -t6 * t
     #117 / 0.16D2)
      t126 = x2 * s * t1
      t127 = -0.1D1 + x2
      t129 = t127 * s * t1
      t130 = -t127
      t131 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t130, 0.10
     #D1, x4)
      t133 = 0.3141592653589793D1 * t131 * t27
      t136 = FJET(XB1, XB2, s, 0.0D0, t126, 0.0D0, -t129, 0.0D0, -t6 * t
     #133 / 0.16D2)
      t141 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t130, 0.10
     #D1, x4)
      t143 = 0.3141592653589793D1 * t141 * t27
      t146 = FJET(XB1, XB2, s, 0.0D0, -t129, 0.0D0, t126, 0.0D0, -t6 * t
     #143 / 0.16D2)
      t151 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, t114, 0.10D1, 0.10
     #D1, x4)
      t153 = 0.3141592653589793D1 * t151 * t31
      t156 = FJET(XB1, XB2, s, 0.0D0, -t109, t103, 0.0D0, -t113, -t6 * t
     #153 / 0.16D2)
      t161 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t166 = 0.3141592653589793D1 * t161
      t173 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.
     #10D1, x4)
      t179 = -t7 * t161 * t20 * t22 / 0.32D2 + t6 * t166 * t27 / 0.16D2 
     #+ t6 * t166 * t31 / 0.16D2 + t6 * 0.3141592653589793D1 * t173 / 0.
     #32D2 + t55 * t161 / 0.2880D4
      t180 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t179)
      t182 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, t114, 0.10D1, 0.10
     #D1, x4)
      t184 = 0.3141592653589793D1 * t182 * t31
      t187 = FJET(XB1, XB2, s, t103, 0.0D0, 0.0D0, -t109, -t113, -t6 * t
     #184 / 0.16D2)
      t192 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t130, 0.10
     #D1, x4)
      t194 = 0.3141592653589793D1 * t192 * t27
      t197 = FJET(XB1, XB2, s, t126, 0.0D0, -t129, 0.0D0, 0.0D0, -t6 * t
     #194 / 0.16D2)
      t202 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t130, 0.10
     #D1, x4)
      t204 = 0.3141592653589793D1 * t202 * t27
      t207 = FJET(XB1, XB2, s, -t129, 0.0D0, t126, 0.0D0, 0.0D0, -t6 * t
     #204 / 0.16D2)
      t212 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, t114, 0.10D1, 0.10
     #D1, x4)
      t214 = 0.3141592653589793D1 * t212 * t31
      t217 = FJET(XB1, XB2, s, -t109, 0.0D0, 0.0D0, t103, -t113, -t6 * t
     #214 / 0.16D2)
      rrqg2qght8s1em2 = t59 * t58 + t80 * t79 + t101 * t100 - t120 * t3 
     #* t5 * t117 / 0.16D2 - t136 * t3 * t5 * t133 / 0.16D2 - t146 * t3 
     #* t5 * t143 / 0.16D2 - t156 * t3 * t5 * t153 / 0.16D2 + t180 * t17
     #9 - t187 * t3 * t5 * t184 / 0.16D2 - t197 * t3 * t5 * t194 / 0.16D
     #2 - t207 * t3 * t5 * t204 / 0.16D2 - t217 * t3 * t5 * t214 / 0.16D
     #2

      end function



      doubleprecision function rrqg2qght8s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.10
     #D1, x4)
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t6 * 0.314
     #1592653589793D1 * t7 / 0.32D2)
      t13 = t5 * 0.3141592653589793D1
      t16 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t20 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t6 * 0.314
     #1592653589793D1 * t16 / 0.32D2)
      t24 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t28 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t6 * 0.314
     #1592653589793D1 * t24 / 0.32D2)
      t32 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1, 0.1
     #0D1, x4)
      t36 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t6 * 0.314
     #1592653589793D1 * t32 / 0.32D2)
      rrqg2qght8s1em3 = t11 * t3 * t13 * t7 / 0.32D2 + t20 * t3 * t13 * 
     #t16 / 0.32D2 + t28 * t3 * t13 * t24 / 0.32D2 + t36 * t3 * t13 * t3
     #2 / 0.32D2

      end function



      doubleprecision function rrqg2qght8s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6
      rrqg2qght8s1em4 = 0.0D0

      end function


      doubleprecision function rrqg2qght8s2e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x3
      t4 = -0.1D1 + x3
      t5 = t2 * t4
      t6 = 0.1D1 / t1
      t7 = s ** 2
      t8 = 0.1D1 / t7
      t9 = t6 * t8
      t10 = lh ** 2
      t12 = 0.3141592653589793D1 ** 2
      t14 = 0.180D3 * t10 - 0.30D2 * t12
      t15 = t14 * 0.3141592653589793D1
      t16 = t1 ** 2
      t17 = t16 ** 2
      t19 = x4 * 0.3141592653589793D1
      t20 = Sin(t19)
      t21 = t20 ** 2
      t22 = z ** 2
      t23 = 0.1D1 / t22
      t24 = t21 * t23
      t25 = t24 * t4
      t28 = log(-0.4D1 * x3 * t17 * t25)
      t29 = -t4
      t30 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t29,
     # x4)
      t32 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t29,
     # x4)
      t36 = t28 ** 2
      t39 = rrqg2qgh82J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t29,
     # x4)
      t40 = t36 * t28
      t43 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t29,
     # x4)
      t53 = 0.60D2 * lh * t12 - 0.2884936567583026D3 - 0.120D3 * t10 * l
     #h
      t54 = t53 * 0.3141592653589793D1
      t57 = 0.3141592653589793D1 * lh
      t66 = 0.1D1 / x3
      t69 = x2 * x3
      t70 = t69 * t17
      t73 = log(-0.4D1 * t70 * t25)
      t74 = t73 ** 2
      t88 = t9 * t15 * t30
      t91 = 0.1D1 / x2
      t94 = x1 ** 2
      t95 = x3 * t94
      t99 = log(-0.4D1 * t95 * t17 * t25)
      t100 = t99 ** 2
      t115 = 0.1D1 / x1
      t118 = t95 * x2
      t119 = t17 * t21
      t124 = log(-0.4D1 * t118 * t119 * t23 * t4)
      t135 = t91 * t115
      t138 = (t9 * t15 * (t28 * t30 - t32) + 0.90D2 * t9 * 0.31415926535
     #89793D1 * (-t36 * t32 / 0.2D1 - t39 + t40 * t30 / 0.6D1 + t28 * t4
     #3) - t9 * t54 * t30 - 0.180D3 * t9 * t57 * (t28 * t32 - t36 * t30 
     #/ 0.2D1 - t43)) * t66 / 0.1440D4 + (0.90D2 * t9 * 0.31415926535897
     #93D1 * (-t43 - t74 * t30 / 0.2D1 + t73 * t32) - 0.180D3 * t9 * t57
     # * (t73 * t30 - t32) - t88) * t66 * t91 / 0.1440D4 + (0.90D2 * t9 
     #* 0.3141592653589793D1 * (-t100 * t30 / 0.2D1 + t99 * t32 - t43) -
     # 0.180D3 * t9 * t57 * (t99 * t30 - t32) - t88) * t66 * t115 / 0.72
     #0D3 + (0.90D2 * t9 * 0.3141592653589793D1 * (t124 * t30 - t32) + 0
     #.180D3 * t9 * t57 * t30) * t66 * t135 / 0.720D3
      t139 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t138)
      t141 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t29
     #, x4)
      t143 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t29
     #, x4)
      t149 = rrqg2qgh81J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t29
     #, x4)
      t152 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t29
     #, x4)
      t183 = t9 * t15 * t141
      t216 = (t9 * t15 * (t28 * t141 - t143) + 0.90D2 * t9 * 0.314159265
     #3589793D1 * (-t36 * t143 / 0.2D1 - t149 + t40 * t141 / 0.6D1 + t28
     # * t152) - t9 * t54 * t141 - 0.180D3 * t9 * t57 * (t28 * t143 - t3
     #6 * t141 / 0.2D1 - t152)) * t66 / 0.1440D4 + (0.90D2 * t9 * 0.3141
     #592653589793D1 * (-t74 * t141 / 0.2D1 - t152 + t73 * t143) - 0.180
     #D3 * t9 * t57 * (t73 * t141 - t143) - t183) * t66 * t91 / 0.1440D4
     # + (0.90D2 * t9 * 0.3141592653589793D1 * (t99 * t143 - t100 * t141
     # / 0.2D1 - t152) - 0.180D3 * t9 * t57 * (-t143 + t99 * t141) - t18
     #3) * t66 * t115 / 0.720D3 + (0.90D2 * t9 * 0.3141592653589793D1 * 
     #(-t143 + t124 * t141) + 0.180D3 * t9 * t57 * t141) * t66 * t135 / 
     #0.720D3
      t217 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, t216)
      t219 = -0.1D1 + x2
      t220 = t219 * s
      t221 = t220 * t1
      t223 = x2 * s * t1
      t224 = t219 ** 2
      t225 = t24 * t224
      t228 = log(0.4D1 * t70 * t225)
      t229 = t228 ** 2
      t230 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t233 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t234 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t246 = t9 * t15 * t230
      t251 = x2 * t17
      t254 = log(0.4D1 * t251 * t225)
      t259 = t254 ** 2
      t263 = t259 * t254
      t266 = rrqg2qgh81J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t283 = t23 * t224
      t287 = log(0.4D1 * t118 * t119 * t283)
      t300 = x2 * t94
      t301 = t300 * t17
      t304 = log(0.4D1 * t301 * t225)
      t306 = t304 ** 2
      t322 = (0.90D2 * t9 * 0.3141592653589793D1 * (-t229 * t230 / 0.2D1
     # - t233 + t228 * t234) - 0.180D3 * t9 * t57 * (t228 * t230 - t234)
     # - t246) * t66 * t91 / 0.1440D4 - (t9 * t15 * (t234 - t254 * t230)
     # + 0.90D2 * t9 * 0.3141592653589793D1 * (t259 * t234 / 0.2D1 - t25
     #4 * t233 - t263 * t230 / 0.6D1 + t266) + t9 * t54 * t230 - 0.180D3
     # * t9 * t57 * (-t254 * t234 + t233 + t259 * t230 / 0.2D1)) * t91 /
     # 0.1440D4 + (0.90D2 * t9 * 0.3141592653589793D1 * (-t234 + t287 * 
     #t230) + 0.180D3 * t9 * t57 * t230) * t66 * t135 / 0.720D3 + (0.90D
     #2 * t9 * 0.3141592653589793D1 * (-t233 + t304 * t234 - t306 * t230
     # / 0.2D1) - 0.180D3 * t9 * t57 * (t304 * t230 - t234) - t246) * t9
     #1 * t115 / 0.720D3
      t323 = FJET(XB1, XB2, s, -t221, 0.0D0, t223, 0.0D0, 0.0D0, t322)
      t325 = t2 * x1
      t326 = -0.1D1 + x1
      t327 = t1 * t326
      t328 = t220 * t327
      t330 = x1 * z
      t331 = 0.1D1 - x1 + t330
      t332 = 0.1D1 / t331
      t334 = t2 * t326 * x2 * t332
      t339 = s * t16 * x2 * x1 * t326 * t332
      t340 = -t326
      t341 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, t340, x2, 0.10D1, 
     #x4)
      t343 = t69 * t94 * t17
      t344 = t326 ** 2
      t345 = t332 * t344
      t350 = log(0.4D1 * t343 * t24 * t345 * t224)
      t351 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, t340, x2, 0.10D1, 
     #x4)
      t364 = t23 * t332
      t369 = log(0.4D1 * t300 * t119 * t364 * t344 * t224)
      t371 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, t340, x2, 0.10D1, 
     #x4)
      t372 = t369 ** 2
      t390 = (0.90D2 * t9 * 0.3141592653589793D1 * (t341 - t350 * t351) 
     #- 0.180D3 * t9 * t57 * t351) * t66 * t135 / 0.720D3 + (0.90D2 * t9
     # * 0.3141592653589793D1 * (-t369 * t341 + t371 + t372 * t351 / 0.2
     #D1) - 0.180D3 * t9 * t57 * (t341 - t369 * t351) + t9 * t15 * t351)
     # * t91 * t115 / 0.720D3
      t391 = FJET(XB1, XB2, s, t325, t328, 0.0D0, -t334, -t339, t390)
      t393 = t2 * t326
      t395 = t23 * t17
      t399 = log(0.4D1 * t95 * t21 * t395 * t345)
      t400 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, t340, 0.0D0, 0.10D
     #1, x4)
      t402 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, t340, 0.0D0, 0.10D
     #1, x4)
      t403 = t399 ** 2
      t404 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, t340, 0.0D0, 0.10D
     #1, x4)
      t417 = t9 * t15 * t404
      t421 = t94 * t21
      t427 = log(0.4D1 * t421 * t23 * t17 * t332 * t344)
      t432 = t427 ** 2
      t436 = t432 * t427
      t439 = rrqg2qgh82J4(s, XB1, XB2, z, lh, wd, nf, t340, 0.0D0, 0.10D
     #1, x4)
      t455 = t24 * t345
      t458 = log(0.4D1 * t343 * t455)
      t472 = log(0.4D1 * t301 * t455)
      t473 = t472 ** 2
      t490 = (0.90D2 * t9 * 0.3141592653589793D1 * (t399 * t400 - t402 -
     # t403 * t404 / 0.2D1) - 0.180D3 * t9 * t57 * (t399 * t404 - t400) 
     #- t417) * t66 * t115 / 0.720D3 + (t9 * t15 * (-t400 + t427 * t404)
     # + 0.90D2 * t9 * 0.3141592653589793D1 * (-t432 * t400 / 0.2D1 + t4
     #27 * t402 + t436 * t404 / 0.6D1 - t439) - t9 * t54 * t404 - 0.180D
     #3 * t9 * t57 * (-t402 - t432 * t404 / 0.2D1 + t427 * t400)) * t115
     # / 0.720D3 + (0.90D2 * t9 * 0.3141592653589793D1 * (-t400 + t458 *
     # t404) + 0.180D3 * t9 * t57 * t404) * t66 * t135 / 0.720D3 + (0.90
     #D2 * t9 * 0.3141592653589793D1 * (-t402 - t473 * t404 / 0.2D1 + t4
     #72 * t400) - 0.180D3 * t9 * t57 * (-t400 + t472 * t404) - t417) * 
     #t91 * t115 / 0.720D3
      t491 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t393, t325, 0.0D0, t490)
      t493 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t495 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t509 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t521 = t9 * t15 * t493
      t546 = rrqg2qgh83J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t568 = (0.90D2 * t9 * 0.3141592653589793D1 * (t287 * t493 - t495) 
     #+ 0.180D3 * t9 * t57 * t493) * t66 * t135 / 0.720D3 + (0.90D2 * t9
     # * 0.3141592653589793D1 * (-t306 * t493 / 0.2D1 - t509 + t304 * t4
     #95) - 0.180D3 * t9 * t57 * (t304 * t493 - t495) - t521) * t91 * t1
     #15 / 0.720D3 + (0.90D2 * t9 * 0.3141592653589793D1 * (-t229 * t493
     # / 0.2D1 - t509 + t228 * t495) - 0.180D3 * t9 * t57 * (-t495 + t22
     #8 * t493) - t521) * t66 * t91 / 0.1440D4 - (t9 * t15 * (t495 - t25
     #4 * t493) + 0.90D2 * t9 * 0.3141592653589793D1 * (t546 + t259 * t4
     #95 / 0.2D1 - t263 * t493 / 0.6D1 - t254 * t509) + t9 * t54 * t493 
     #- 0.180D3 * t9 * t57 * (t259 * t493 / 0.2D1 + t509 - t254 * t495))
     # * t91 / 0.1440D4
      t569 = FJET(XB1, XB2, s, 0.0D0, -t221, 0.0D0, t223, 0.0D0, t568)
      t571 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, t340, x2, 0.10D1, 
     #x4)
      t573 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, t340, x2, 0.10D1, 
     #x4)
      t585 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, t340, x2, 0.10D1, 
     #x4)
      t603 = (0.90D2 * t9 * 0.3141592653589793D1 * (-t350 * t571 + t573)
     # - 0.180D3 * t9 * t57 * t571) * t66 * t135 / 0.720D3 + (0.90D2 * t
     #9 * 0.3141592653589793D1 * (-t369 * t573 + t585 + t372 * t571 / 0.
     #2D1) - 0.180D3 * t9 * t57 * (-t369 * t571 + t573) + t9 * t15 * t57
     #1) * t91 * t115 / 0.720D3
      t604 = FJET(XB1, XB2, s, 0.0D0, -t334, t325, t328, -t339, t603)
      t606 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, t340, x2, 0.10D1, 
     #x4)
      t607 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, t340, x2, 0.10D1, 
     #x4)
      t622 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, t340, x2, 0.10D1, 
     #x4)
      t638 = (0.90D2 * t9 * 0.3141592653589793D1 * (t606 - t350 * t607) 
     #- 0.180D3 * t9 * t57 * t607) * t66 * t135 / 0.720D3 + (0.90D2 * t9
     # * 0.3141592653589793D1 * (t372 * t607 / 0.2D1 - t369 * t606 + t62
     #2) - 0.180D3 * t9 * t57 * (t606 - t369 * t607) + t9 * t15 * t607) 
     #* t91 * t115 / 0.720D3
      t639 = FJET(XB1, XB2, s, -t334, 0.0D0, t328, t325, -t339, t638)
      t641 = rrqg2qgh84J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t645 = t24 * t17
      t647 = log(0.4D1 * t645)
      t648 = t647 ** 2
      t649 = t648 * t6
      t650 = t8 * lh
      t655 = t648 * t647 * t6
      t658 = t647 * t6
      t659 = t8 * t14
      t662 = (-0.90D2 * t649 * t650 + t9 * t53 - 0.15D2 * t655 * t8 - t6
     #58 * t659) * 0.3141592653589793D1
      t663 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t670 = t9 * t14
      t672 = (0.180D3 * t658 * t650 + 0.45D2 * t649 * t8 + t670) * 0.314
     #1592653589793D1
      t673 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t676 = t9 * lh
      t681 = (-0.180D3 * t676 - 0.90D2 * t658 * t8) * 0.3141592653589793
     #D1
      t682 = rrqg2qgh84J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t691 = t12 ** 2
      t692 = t10 ** 2
      t699 = t648 ** 2
      t704 = (0.30D2 * t655 * t650 + t649 * t659 / 0.2D1 - t658 * t8 * t
     #53 + t9 * (t691 + 0.60D2 * t692 + 0.5769873135166051D3 * lh - 0.60
     #D2 * t10 * t12) + 0.15D2 / 0.4D1 * t699 * t6 * t8) * 0.31415926535
     #89793D1
      t705 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t710 = log(0.4D1 * t95 * t645)
      t711 = t710 ** 2
      t725 = t9 * t15 * t705
      t732 = log(0.4D1 * t421 * t395)
      t737 = t732 ** 2
      t740 = t737 * t732
      t749 = t9 * t54 * t705
      t762 = log(0.4D1 * t118 * t645)
      t777 = log(0.4D1 * t300 * t645)
      t778 = t777 ** 2
      t798 = log(0.4D1 * x3 * t21 * t395)
      t803 = t798 ** 2
      t806 = t803 * t798
      t826 = log(0.4D1 * t69 * t645)
      t827 = t826 ** 2
      t846 = log(0.4D1 * t251 * t24)
      t851 = t846 ** 2
      t852 = t851 * t846
      t872 = t9 * 0.3141592653589793D1 * t641 / 0.16D2 + t662 * t663 / 0
     #.1440D4 + t672 * t673 / 0.1440D4 + t681 * t682 / 0.1440D4 + t704 *
     # t705 / 0.1440D4 + (0.90D2 * t9 * 0.3141592653589793D1 * (t673 + t
     #711 * t705 / 0.2D1 - t710 * t663) - 0.180D3 * t9 * t57 * (t663 - t
     #710 * t705) + t725) * t66 * t115 / 0.720D3 + (t9 * t15 * (t663 - t
     #732 * t705) + 0.90D2 * t9 * 0.3141592653589793D1 * (t737 * t663 / 
     #0.2D1 + t682 - t740 * t705 / 0.6D1 - t732 * t673) + t749 - 0.180D3
     # * t9 * t57 * (t673 + t737 * t705 / 0.2D1 - t732 * t663)) * t115 /
     # 0.720D3 + (0.90D2 * t9 * 0.3141592653589793D1 * (t663 - t762 * t7
     #05) - 0.180D3 * t9 * t57 * t705) * t66 * t135 / 0.720D3 + (0.90D2 
     #* t9 * 0.3141592653589793D1 * (t673 + t778 * t705 / 0.2D1 - t777 *
     # t663) - 0.180D3 * t9 * t57 * (t663 - t777 * t705) + t725) * t91 *
     # t115 / 0.720D3 + (t9 * t15 * (t663 - t798 * t705) + 0.90D2 * t9 *
     # 0.3141592653589793D1 * (t803 * t663 / 0.2D1 + t682 - t806 * t705 
     #/ 0.6D1 - t798 * t673) + t749 - 0.180D3 * t9 * t57 * (t673 + t803 
     #* t705 / 0.2D1 - t798 * t663)) * t66 / 0.1440D4 + (0.90D2 * t9 * 0
     #.3141592653589793D1 * (t673 + t827 * t705 / 0.2D1 - t826 * t663) -
     # 0.180D3 * t9 * t57 * (t663 - t826 * t705) + t725) * t66 * t91 / 0
     #.1440D4 - (t9 * t15 * (-t663 + t846 * t705) + 0.90D2 * t9 * 0.3141
     #592653589793D1 * (t852 * t705 / 0.6D1 + t846 * t673 - t851 * t663 
     #/ 0.2D1 - t682) - t749 - 0.180D3 * t9 * t57 * (-t673 + t846 * t663
     # - t851 * t705 / 0.2D1)) * t91 / 0.1440D4
      t873 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t872)
      t875 = 0.3D1 * t69
      t876 = x2 ** 2
      t877 = t876 * x3
      t878 = cos(t19)
      t880 = Sqrt(-t69 * t4)
      t881 = t878 * t880
      t882 = 0.2D1 * t881
      t884 = 0.2D1 * t881 * x2
      t886 = -0.1D1 + t69
      t887 = 0.1D1 / t886
      t889 = t2 * (-x2 - x3 + t875 - t877 - t882 + t884) * t887
      t893 = t2 * t219 * (-t69 - 0.1D1 + x3 + t882) * t887
      t894 = x2 * z
      t895 = 0.1D1 - x2 + t894
      t896 = t4 * t887
      t897 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t896, 
     #x4)
      t898 = t895 * t897
      t899 = t224 * t4
      t900 = t886 ** 2
      t901 = 0.1D1 / t900
      t906 = log(-0.4D1 * t343 * t24 * t899 * t901)
      t907 = t906 * t895
      t908 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t896, 
     #x4)
      t912 = t877 * z
      t913 = t69 * z
      t914 = 0.2D1 * t69
      t918 = 0.1D1 / (t877 - t912 - t894 + t913 - t884 + x2 - t914 + 0.2
     #D1 * t881 * t894 - 0.1D1 + t882)
      t922 = 0.3141592653589793D1 * t895
      t924 = t922 * t908 * t918
      t931 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t896, 
     #x4)
      t938 = log(-0.4D1 * t69 * t119 * t283 * t4 * t901)
      t939 = t938 * t895
      t941 = t938 ** 2
      t942 = t941 * t895
      t961 = (-0.90D2 * t9 * 0.3141592653589793D1 * (t898 - t907 * t908)
     # * t918 + 0.180D3 * t676 * t924) * t66 * t135 / 0.720D3 + (-0.90D2
     # * t9 * 0.3141592653589793D1 * (t895 * t931 - t939 * t897 + t942 *
     # t908 / 0.2D1) * t918 + 0.180D3 * t676 * 0.3141592653589793D1 * (t
     #898 - t939 * t908) * t918 - t670 * t924) * t66 * t91 / 0.1440D4
      t962 = FJET(XB1, XB2, s, 0.0D0, t889, 0.0D0, -t893, 0.0D0, t961)
      t964 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t967 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t969 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t980 = t9 * t15 * t964
      t989 = rrqg2qgh82J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t1039 = (0.90D2 * t9 * 0.3141592653589793D1 * (-t229 * t964 / 0.2D
     #1 + t228 * t967 - t969) - 0.180D3 * t9 * t57 * (t228 * t964 - t967
     #) - t980) * t66 * t91 / 0.1440D4 - (t9 * t15 * (t967 - t254 * t964
     #) + 0.90D2 * t9 * 0.3141592653589793D1 * (t989 - t263 * t964 / 0.6
     #D1 + t259 * t967 / 0.2D1 - t254 * t969) + t9 * t54 * t964 - 0.180D
     #3 * t9 * t57 * (-t254 * t967 + t969 + t259 * t964 / 0.2D1)) * t91 
     #/ 0.1440D4 + (0.90D2 * t9 * 0.3141592653589793D1 * (t287 * t964 - 
     #t967) + 0.180D3 * t9 * t57 * t964) * t66 * t135 / 0.720D3 + (0.90D
     #2 * t9 * 0.3141592653589793D1 * (t304 * t967 - t306 * t964 / 0.2D1
     # - t969) - 0.180D3 * t9 * t57 * (t304 * t964 - t967) - t980) * t91
     # * t115 / 0.720D3
      t1040 = FJET(XB1, XB2, s, t223, 0.0D0, -t221, 0.0D0, 0.0D0, t1039)
      t1042 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t2
     #9, x4)
      t1043 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t2
     #9, x4)
      t1050 = rrqg2qgh83J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t2
     #9, x4)
      t1051 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t2
     #9, x4)
      t1084 = t9 * t15 * t1043
      t1117 = (t9 * t15 * (-t1042 + t28 * t1043) + 0.90D2 * t9 * 0.31415
     #92653589793D1 * (-t36 * t1042 / 0.2D1 - t1050 + t28 * t1051 + t40 
     #* t1043 / 0.6D1) - t9 * t54 * t1043 - 0.180D3 * t9 * t57 * (-t36 *
     # t1043 / 0.2D1 - t1051 + t28 * t1042)) * t66 / 0.1440D4 + (0.90D2 
     #* t9 * 0.3141592653589793D1 * (-t1051 - t100 * t1043 / 0.2D1 + t99
     # * t1042) - 0.180D3 * t9 * t57 * (-t1042 + t99 * t1043) - t1084) *
     # t66 * t115 / 0.720D3 + (0.90D2 * t9 * 0.3141592653589793D1 * (-t1
     #042 + t124 * t1043) + 0.180D3 * t9 * t57 * t1043) * t66 * t135 / 0
     #.720D3 + (0.90D2 * t9 * 0.3141592653589793D1 * (-t74 * t1043 / 0.2
     #D1 + t73 * t1042 - t1051) - 0.180D3 * t9 * t57 * (t73 * t1043 - t1
     #042) - t1084) * t66 * t91 / 0.1440D4
      t1118 = FJET(XB1, XB2, s, 0.0D0, -t5, 0.0D0, t3, 0.0D0, t1117)
      t1120 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t1122 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t1123 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t1136 = t9 * t15 * t1123
      t1145 = rrqg2qgh82J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t1156 = t9 * t54 * t1123
      t1167 = rrqg2qgh82J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t1269 = (0.90D2 * t9 * 0.3141592653589793D1 * (-t826 * t1120 + t11
     #22 + t827 * t1123 / 0.2D1) - 0.180D3 * t9 * t57 * (t1120 - t826 * 
     #t1123) + t1136) * t66 * t91 / 0.1440D4 - (t9 * t15 * (-t1120 + t84
     #6 * t1123) + 0.90D2 * t9 * 0.3141592653589793D1 * (-t1145 + t852 *
     # t1123 / 0.6D1 - t851 * t1120 / 0.2D1 + t846 * t1122) - t1156 - 0.
     #180D3 * t9 * t57 * (-t1122 + t846 * t1120 - t851 * t1123 / 0.2D1))
     # * t91 / 0.1440D4 + t9 * 0.3141592653589793D1 * t1167 / 0.16D2 + t
     #662 * t1120 / 0.1440D4 + t672 * t1122 / 0.1440D4 + t681 * t1145 / 
     #0.1440D4 + t704 * t1123 / 0.1440D4 + (t9 * t15 * (t1120 - t798 * t
     #1123) + 0.90D2 * t9 * 0.3141592653589793D1 * (-t806 * t1123 / 0.6D
     #1 + t803 * t1120 / 0.2D1 + t1145 - t798 * t1122) + t1156 - 0.180D3
     # * t9 * t57 * (-t798 * t1120 + t1122 + t803 * t1123 / 0.2D1)) * t6
     #6 / 0.1440D4 + (0.90D2 * t9 * 0.3141592653589793D1 * (-t710 * t112
     #0 + t1122 + t711 * t1123 / 0.2D1) - 0.180D3 * t9 * t57 * (t1120 - 
     #t710 * t1123) + t1136) * t66 * t115 / 0.720D3 + (t9 * t15 * (t1120
     # - t732 * t1123) + 0.90D2 * t9 * 0.3141592653589793D1 * (-t740 * t
     #1123 / 0.6D1 + t737 * t1120 / 0.2D1 + t1145 - t732 * t1122) + t115
     #6 - 0.180D3 * t9 * t57 * (-t732 * t1120 + t1122 + t737 * t1123 / 0
     #.2D1)) * t115 / 0.720D3 + (0.90D2 * t9 * 0.3141592653589793D1 * (t
     #1120 - t762 * t1123) - 0.180D3 * t9 * t57 * t1123) * t66 * t135 / 
     #0.720D3 + (0.90D2 * t9 * 0.3141592653589793D1 * (-t777 * t1120 + t
     #1122 + t778 * t1123 / 0.2D1) - 0.180D3 * t9 * t57 * (t1120 - t777 
     #* t1123) + t1136) * t91 * t115 / 0.720D3
      t1270 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t1269)
      t1272 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t1275 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t1276 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t1288 = t9 * t15 * t1272
      t1298 = rrqg2qgh83J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t1308 = t9 * t54 * t1272
      t1390 = rrqg2qgh83J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t1421 = (0.90D2 * t9 * 0.3141592653589793D1 * (t711 * t1272 / 0.2D
     #1 + t1275 - t710 * t1276) - 0.180D3 * t9 * t57 * (-t710 * t1272 + 
     #t1276) + t1288) * t66 * t115 / 0.720D3 + (t9 * t15 * (t1276 - t732
     # * t1272) + 0.90D2 * t9 * 0.3141592653589793D1 * (-t732 * t1275 + 
     #t1298 + t737 * t1276 / 0.2D1 - t740 * t1272 / 0.6D1) + t1308 - 0.1
     #80D3 * t9 * t57 * (t1275 + t737 * t1272 / 0.2D1 - t732 * t1276)) *
     # t115 / 0.720D3 + (0.90D2 * t9 * 0.3141592653589793D1 * (t1276 - t
     #762 * t1272) - 0.180D3 * t9 * t57 * t1272) * t66 * t135 / 0.720D3 
     #+ (0.90D2 * t9 * 0.3141592653589793D1 * (t1275 + t778 * t1272 / 0.
     #2D1 - t777 * t1276) - 0.180D3 * t9 * t57 * (t1276 - t777 * t1272) 
     #+ t1288) * t91 * t115 / 0.720D3 + t662 * t1276 / 0.1440D4 + t672 *
     # t1275 / 0.1440D4 + (0.90D2 * t9 * 0.3141592653589793D1 * (-t826 *
     # t1276 + t1275 + t827 * t1272 / 0.2D1) - 0.180D3 * t9 * t57 * (-t8
     #26 * t1272 + t1276) + t1288) * t66 * t91 / 0.1440D4 - (t9 * t15 * 
     #(-t1276 + t846 * t1272) + 0.90D2 * t9 * 0.3141592653589793D1 * (t8
     #52 * t1272 / 0.6D1 + t846 * t1275 - t1298 - t851 * t1276 / 0.2D1) 
     #- t1308 - 0.180D3 * t9 * t57 * (t846 * t1276 - t1275 - t851 * t127
     #2 / 0.2D1)) * t91 / 0.1440D4 + t9 * 0.3141592653589793D1 * t1390 /
     # 0.16D2 + t681 * t1298 / 0.1440D4 + (t9 * t15 * (-t798 * t1272 + t
     #1276) + 0.90D2 * t9 * 0.3141592653589793D1 * (t803 * t1276 / 0.2D1
     # - t806 * t1272 / 0.6D1 + t1298 - t798 * t1275) + t1308 - 0.180D3 
     #* t9 * t57 * (-t798 * t1276 + t1275 + t803 * t1272 / 0.2D1)) * t66
     # / 0.1440D4 + t704 * t1272 / 0.1440D4
      t1422 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t1421)
      t1424 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t2
     #9, x4)
      t1425 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t2
     #9, x4)
      t1428 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t2
     #9, x4)
      t1440 = t9 * t15 * t1425
      t1479 = rrqg2qgh84J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t2
     #9, x4)
      t1499 = (0.90D2 * t9 * 0.3141592653589793D1 * (-t1424 - t100 * t14
     #25 / 0.2D1 + t99 * t1428) - 0.180D3 * t9 * t57 * (t99 * t1425 - t1
     #428) - t1440) * t66 * t115 / 0.720D3 + (0.90D2 * t9 * 0.3141592653
     #589793D1 * (-t1428 + t124 * t1425) + 0.180D3 * t9 * t57 * t1425) *
     # t66 * t135 / 0.720D3 + (0.90D2 * t9 * 0.3141592653589793D1 * (-t1
     #424 - t74 * t1425 / 0.2D1 + t73 * t1428) - 0.180D3 * t9 * t57 * (t
     #73 * t1425 - t1428) - t1440) * t66 * t91 / 0.1440D4 + (t9 * t15 * 
     #(t28 * t1425 - t1428) + 0.90D2 * t9 * 0.3141592653589793D1 * (-t36
     # * t1428 / 0.2D1 - t1479 + t40 * t1425 / 0.6D1 + t28 * t1424) - t9
     # * t54 * t1425 - 0.180D3 * t9 * t57 * (-t36 * t1425 / 0.2D1 + t28 
     #* t1428 - t1424)) * t66 / 0.1440D4
      t1500 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t1499)
      t1502 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t896,
     # x4)
      t1504 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t896,
     # x4)
      t1506 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t896,
     # x4)
      t1514 = t895 * t1504
      t1522 = t922 * t1506 * t918
      t1540 = (-0.90D2 * t9 * 0.3141592653589793D1 * (t895 * t1502 - t93
     #9 * t1504 + t942 * t1506 / 0.2D1) * t918 + 0.180D3 * t676 * 0.3141
     #592653589793D1 * (t1514 - t939 * t1506) * t918 - t670 * t1522) * t
     #66 * t91 / 0.1440D4 + (-0.90D2 * t9 * 0.3141592653589793D1 * (t151
     #4 - t907 * t1506) * t918 + 0.180D3 * t676 * t1522) * t66 * t135 / 
     #0.720D3
      t1541 = FJET(XB1, XB2, s, t889, 0.0D0, -t893, 0.0D0, 0.0D0, t1540)
      t1543 = t139 * t138 + t217 * t216 + t323 * t322 + t391 * t390 + t4
     #91 * t490 + t569 * t568 + t604 * t603 + t639 * t638 + t873 * t872 
     #+ t962 * t961 + t1040 * t1039 + t1118 * t1117 + t1270 * t1269 + t1
     #422 * t1421 + t1500 * t1499 + t1541 * t1540
      t1544 = t4 * s
      t1545 = t1 * x1
      t1546 = t1544 * t1545
      t1547 = t1544 * t327
      t1548 = x3 * x1
      t1549 = t2 * t1548
      t1551 = x3 * s * t327
      t1557 = log(-0.4D1 * t95 * t119 * t364 * t344 * t4)
      t1558 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, t340, 0.0D0, t29,
     # x4)
      t1560 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, t340, 0.0D0, t29,
     # x4)
      t1561 = t1557 ** 2
      t1562 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, t340, 0.0D0, t29,
     # x4)
      t1583 = log(-0.4D1 * t343 * t24 * t345 * t4)
      t1596 = (0.90D2 * t9 * 0.3141592653589793D1 * (-t1557 * t1558 + t1
     #560 + t1561 * t1562 / 0.2D1) - 0.180D3 * t9 * t57 * (-t1557 * t156
     #2 + t1558) + t9 * t15 * t1562) * t66 * t115 / 0.720D3 + (0.90D2 * 
     #t9 * 0.3141592653589793D1 * (-t1583 * t1562 + t1558) - 0.180D3 * t
     #9 * t57 * t1562) * t66 * t135 / 0.720D3
      t1597 = FJET(XB1, XB2, s, -t1546, t1547, t1549, -t1551, 0.0D0, t15
     #96)
      t1599 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t1600 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t1607 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t1611 = rrqg2qgh81J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t1617 = t9 * t54 * t1600
      t1643 = t9 * t15 * t1600
      t1742 = rrqg2qgh81J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t1748 = (t9 * t15 * (t1599 - t798 * t1600) + 0.90D2 * t9 * 0.31415
     #92653589793D1 * (t803 * t1599 / 0.2D1 - t798 * t1607 - t806 * t160
     #0 / 0.6D1 + t1611) + t1617 - 0.180D3 * t9 * t57 * (t1607 - t798 * 
     #t1599 + t803 * t1600 / 0.2D1)) * t66 / 0.1440D4 + t704 * t1600 / 0
     #.1440D4 + (0.90D2 * t9 * 0.3141592653589793D1 * (t711 * t1600 / 0.
     #2D1 + t1607 - t710 * t1599) - 0.180D3 * t9 * t57 * (t1599 - t710 *
     # t1600) + t1643) * t66 * t115 / 0.720D3 + (t9 * t15 * (-t732 * t16
     #00 + t1599) + 0.90D2 * t9 * 0.3141592653589793D1 * (-t732 * t1607 
     #+ t1611 - t740 * t1600 / 0.6D1 + t737 * t1599 / 0.2D1) + t1617 - 0
     #.180D3 * t9 * t57 * (-t732 * t1599 + t1607 + t737 * t1600 / 0.2D1)
     #) * t115 / 0.720D3 + (0.90D2 * t9 * 0.3141592653589793D1 * (-t762 
     #* t1600 + t1599) - 0.180D3 * t9 * t57 * t1600) * t66 * t135 / 0.72
     #0D3 + (0.90D2 * t9 * 0.3141592653589793D1 * (t1607 + t778 * t1600 
     #/ 0.2D1 - t777 * t1599) - 0.180D3 * t9 * t57 * (-t777 * t1600 + t1
     #599) + t1643) * t91 * t115 / 0.720D3 + t662 * t1599 / 0.1440D4 + (
     #0.90D2 * t9 * 0.3141592653589793D1 * (t827 * t1600 / 0.2D1 + t1607
     # - t826 * t1599) - 0.180D3 * t9 * t57 * (-t826 * t1600 + t1599) + 
     #t1643) * t66 * t91 / 0.1440D4 - (t9 * t15 * (-t1599 + t846 * t1600
     #) + 0.90D2 * t9 * 0.3141592653589793D1 * (-t851 * t1599 / 0.2D1 - 
     #t1611 + t846 * t1607 + t852 * t1600 / 0.6D1) - t1617 - 0.180D3 * t
     #9 * t57 * (-t1607 - t851 * t1600 / 0.2D1 + t846 * t1599)) * t91 / 
     #0.1440D4 + t681 * t1611 / 0.1440D4 + t9 * 0.3141592653589793D1 * t
     #1742 / 0.16D2 + t672 * t1607 / 0.1440D4
      t1749 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t1748)
      t1751 = t1548 * z
      t1755 = Sqrt(-x3 * t331 * x2 * t4)
      t1756 = t878 * t1755
      t1757 = 0.2D1 * t1756
      t1762 = t393 * t219 * (-t69 - 0.1D1 + x3 + x1 - t1548 - t330 + t17
     #51 + t1757) * t332 * t887
      t1764 = t1544 * t1545 * t887
      t1766 = 0.2D1 * t1756 * x2
      t1767 = t69 * x1
      t1769 = t877 * x1
      t1770 = t69 * t330
      t1772 = t877 * t330
      t1773 = t1766 - t1751 - 0.2D1 * t1767 + t1769 + t875 + 0.2D1 * t17
     #70 - t1772 - x2 - x3 + t1548 - t877 - t1757
      t1776 = t393 * t1773 * t332 * t887
      t1779 = t325 * x3 * t219 * t887
      t1780 = x2 * x1
      t1796 = 0.1D1 - t877 + t894 + 0.2D1 * t1780 - 0.2D1 * t94 * z + t9
     #4 * t22 - t300 - x2 + t94 + 0.4D1 * t1770 - t1772 - 0.2D1 * t1756 
     #* t1780 - 0.2D1 * t1756 * t330 - 0.2D1 * t1756 * t894 - x3 * t22 *
     # t1780 - 0.2D1 * t95 * t894
      t1800 = t1780 * z
      t1812 = t95 * t22 * x2 - 0.2D1 * x1 + 0.2D1 * t1756 * t1800 - t175
     #7 + t914 + 0.2D1 * t330 + t1766 - 0.3D1 * t1767 + t1769 + 0.2D1 * 
     #t1756 * x1 - 0.3D1 * t1800 + t118 + t1780 * t22 + 0.2D1 * t300 * z
     # - t300 * t22 + t912 - t913
      t1814 = 0.1D1 / (t1796 + t1812)
      t1815 = t331 * t1814
      t1816 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, t340, x2, t896, x
     #4)
      t1827 = log(-0.4D1 * t901 * x2 * x3 * t332 * t17 * t24 * t344 * t8
     #99 * t94)
      t1828 = t1827 * t331
      t1829 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, t340, x2, t896, x
     #4)
      t1834 = -0.1D1 + x2 + x1 - t894 - t1780 + t1800 - t330
      t1838 = t9 * t57
      t1843 = -0.90D2 * t9 * 0.3141592653589793D1 * (-t1815 * t1816 + t1
     #828 * t1814 * t1829) * t1834 - 0.180D3 * t1838 * t1815 * t1829 * t
     #1834
      t1847 = FJET(XB1, XB2, s, t1762, t1764, -t1776, t1779, -t339, t184
     #3 * t66 * t135 / 0.720D3)
      t1850 = t66 * t91 * t115
      t1853 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, t340, x2, t896, x
     #4)
      t1855 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, t340, x2, t896, x
     #4)
      t1867 = -0.90D2 * t9 * 0.3141592653589793D1 * (-t1815 * t1853 + t1
     #828 * t1814 * t1855) * t1834 - 0.180D3 * t1838 * t1815 * t1855 * t
     #1834
      t1871 = FJET(XB1, XB2, s, t1779, -t1776, t1764, t1762, -t339, t186
     #7 * t66 * t135 / 0.720D3)
      t1875 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, t340, x2, t896, x
     #4)
      t1877 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, t340, x2, t896, x
     #4)
      t1889 = -0.90D2 * t9 * 0.3141592653589793D1 * (-t1815 * t1875 + t1
     #828 * t1814 * t1877) * t1834 - 0.180D3 * t1838 * t1815 * t1877 * t
     #1834
      t1893 = FJET(XB1, XB2, s, t1764, t1762, t1779, -t1776, -t339, t188
     #9 * t66 * t135 / 0.720D3)
      t1897 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, t340, x2, t896, x
     #4)
      t1899 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, t340, x2, t896, x
     #4)
      t1911 = -0.90D2 * t9 * 0.3141592653589793D1 * (-t1815 * t1897 + t1
     #828 * t1814 * t1899) * t1834 - 0.180D3 * t1838 * t1815 * t1899 * t
     #1834
      t1915 = FJET(XB1, XB2, s, -t1776, t1779, t1762, t1764, -t339, t191
     #1 * t66 * t135 / 0.720D3)
      t1919 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, t340, 0.0D0, t29,
     # x4)
      t1921 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, t340, 0.0D0, t29,
     # x4)
      t1924 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, t340, 0.0D0, t29,
     # x4)
      t1951 = (0.90D2 * t9 * 0.3141592653589793D1 * (-t1557 * t1919 + t1
     #561 * t1921 / 0.2D1 + t1924) - 0.180D3 * t9 * t57 * (t1919 - t1557
     # * t1921) + t9 * t15 * t1921) * t66 * t115 / 0.720D3 + (0.90D2 * t
     #9 * 0.3141592653589793D1 * (-t1583 * t1921 + t1919) - 0.180D3 * t9
     # * t57 * t1921) * t66 * t135 / 0.720D3
      t1952 = FJET(XB1, XB2, s, t1549, -t1551, -t1546, t1547, 0.0D0, t19
     #51)
      t1954 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, t340, x2, 0.10D1,
     # x4)
      t1956 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, t340, x2, 0.10D1,
     # x4)
      t1967 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, t340, x2, 0.10D1,
     # x4)
      t1986 = (0.90D2 * t9 * 0.3141592653589793D1 * (-t350 * t1954 + t19
     #56) - 0.180D3 * t9 * t57 * t1954) * t66 * t135 / 0.720D3 + (0.90D2
     # * t9 * 0.3141592653589793D1 * (t1967 + t372 * t1954 / 0.2D1 - t36
     #9 * t1956) - 0.180D3 * t9 * t57 * (t1956 - t369 * t1954) + t9 * t1
     #5 * t1954) * t91 * t115 / 0.720D3
      t1987 = FJET(XB1, XB2, s, t328, t325, -t334, 0.0D0, -t339, t1986)
      t1989 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, t340, 0.0D0, t29,
     # x4)
      t1991 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, t340, 0.0D0, t29,
     # x4)
      t1992 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, t340, 0.0D0, t29,
     # x4)
      t2021 = (0.90D2 * t9 * 0.3141592653589793D1 * (-t1557 * t1989 + t1
     #991 + t1561 * t1992 / 0.2D1) - 0.180D3 * t9 * t57 * (-t1557 * t199
     #2 + t1989) + t9 * t15 * t1992) * t66 * t115 / 0.720D3 + (0.90D2 * 
     #t9 * 0.3141592653589793D1 * (-t1583 * t1992 + t1989) - 0.180D3 * t
     #9 * t57 * t1992) * t66 * t135 / 0.720D3
      t2022 = FJET(XB1, XB2, s, t1547, -t1546, -t1551, t1549, 0.0D0, t20
     #21)
      t2024 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, t340, 0.0D0, 0.10
     #D1, x4)
      t2026 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, t340, 0.0D0, 0.10
     #D1, x4)
      t2027 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, t340, 0.0D0, 0.10
     #D1, x4)
      t2040 = t9 * t15 * t2027
      t2050 = rrqg2qgh81J4(s, XB1, XB2, z, lh, wd, nf, t340, 0.0D0, 0.10
     #D1, x4)
      t2096 = (0.90D2 * t9 * 0.3141592653589793D1 * (t399 * t2024 - t202
     #6 - t403 * t2027 / 0.2D1) - 0.180D3 * t9 * t57 * (t399 * t2027 - t
     #2024) - t2040) * t66 * t115 / 0.720D3 + (t9 * t15 * (-t2024 + t427
     # * t2027) + 0.90D2 * t9 * 0.3141592653589793D1 * (-t432 * t2024 / 
     #0.2D1 - t2050 + t427 * t2026 + t436 * t2027 / 0.6D1) - t9 * t54 * 
     #t2027 - 0.180D3 * t9 * t57 * (t427 * t2024 - t2026 - t432 * t2027 
     #/ 0.2D1)) * t115 / 0.720D3 + (0.90D2 * t9 * 0.3141592653589793D1 *
     # (t458 * t2027 - t2024) + 0.180D3 * t9 * t57 * t2027) * t66 * t135
     # / 0.720D3 + (0.90D2 * t9 * 0.3141592653589793D1 * (t472 * t2024 -
     # t2026 - t473 * t2027 / 0.2D1) - 0.180D3 * t9 * t57 * (-t2024 + t4
     #72 * t2027) - t2040) * t91 * t115 / 0.720D3
      t2097 = FJET(XB1, XB2, s, -t393, t325, 0.0D0, 0.0D0, 0.0D0, t2096)
      t2099 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, t340, 0.0D0, t29,
     # x4)
      t2101 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, t340, 0.0D0, t29,
     # x4)
      t2104 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, t340, 0.0D0, t29,
     # x4)
      t2131 = (0.90D2 * t9 * 0.3141592653589793D1 * (-t1557 * t2099 + t1
     #561 * t2101 / 0.2D1 + t2104) - 0.180D3 * t9 * t57 * (-t1557 * t210
     #1 + t2099) + t9 * t15 * t2101) * t66 * t115 / 0.720D3 + (0.90D2 * 
     #t9 * 0.3141592653589793D1 * (t2099 - t1583 * t2101) - 0.180D3 * t9
     # * t57 * t2101) * t66 * t135 / 0.720D3
      t2132 = FJET(XB1, XB2, s, -t1551, t1549, t1547, -t1546, 0.0D0, t21
     #31)
      t2134 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t896,
     # x4)
      t2135 = t895 * t2134
      t2136 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t896,
     # x4)
      t2144 = t922 * t2136 * t918
      t2151 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t896,
     # x4)
      t2172 = (-0.90D2 * t9 * 0.3141592653589793D1 * (t2135 - t907 * t21
     #36) * t918 + 0.180D3 * t676 * t2144) * t66 * t135 / 0.720D3 + (-0.
     #90D2 * t9 * 0.3141592653589793D1 * (t895 * t2151 - t939 * t2134 + 
     #t942 * t2136 / 0.2D1) * t918 + 0.180D3 * t676 * 0.3141592653589793
     #D1 * (t2135 - t939 * t2136) * t918 - t670 * t2144) * t66 * t91 / 0
     #.1440D4
      t2173 = FJET(XB1, XB2, s, 0.0D0, -t893, 0.0D0, t889, 0.0D0, t2172)
      t2175 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t896,
     # x4)
      t2177 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t896,
     # x4)
      t2179 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t896,
     # x4)
      t2187 = t895 * t2177
      t2195 = t922 * t2179 * t918
      t2213 = (-0.90D2 * t9 * 0.3141592653589793D1 * (t895 * t2175 - t93
     #9 * t2177 + t942 * t2179 / 0.2D1) * t918 + 0.180D3 * t676 * 0.3141
     #592653589793D1 * (t2187 - t939 * t2179) * t918 - t670 * t2195) * t
     #66 * t91 / 0.1440D4 + (-0.90D2 * t9 * 0.3141592653589793D1 * (t218
     #7 - t907 * t2179) * t918 + 0.180D3 * t676 * t2195) * t66 * t135 / 
     #0.720D3
      t2214 = FJET(XB1, XB2, s, -t893, 0.0D0, t889, 0.0D0, 0.0D0, t2213)
      t2216 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, t340, 0.0D0, 0.10
     #D1, x4)
      t2219 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, t340, 0.0D0, 0.10
     #D1, x4)
      t2221 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, t340, 0.0D0, 0.10
     #D1, x4)
      t2232 = t9 * t15 * t2216
      t2243 = rrqg2qgh83J4(s, XB1, XB2, z, lh, wd, nf, t340, 0.0D0, 0.10
     #D1, x4)
      t2288 = (0.90D2 * t9 * 0.3141592653589793D1 * (-t403 * t2216 / 0.2
     #D1 + t399 * t2219 - t2221) - 0.180D3 * t9 * t57 * (t399 * t2216 - 
     #t2219) - t2232) * t66 * t115 / 0.720D3 + (t9 * t15 * (-t2219 + t42
     #7 * t2216) + 0.90D2 * t9 * 0.3141592653589793D1 * (-t432 * t2219 /
     # 0.2D1 + t427 * t2221 - t2243 + t436 * t2216 / 0.6D1) - t9 * t54 *
     # t2216 - 0.180D3 * t9 * t57 * (-t2221 - t432 * t2216 / 0.2D1 + t42
     #7 * t2219)) * t115 / 0.720D3 + (0.90D2 * t9 * 0.3141592653589793D1
     # * (t458 * t2216 - t2219) + 0.180D3 * t9 * t57 * t2216) * t66 * t1
     #35 / 0.720D3 + (0.90D2 * t9 * 0.3141592653589793D1 * (t472 * t2219
     # - t2221 - t473 * t2216 / 0.2D1) - 0.180D3 * t9 * t57 * (t472 * t2
     #216 - t2219) - t2232) * t91 * t115 / 0.720D3
      t2289 = FJET(XB1, XB2, s, t325, -t393, 0.0D0, 0.0D0, 0.0D0, t2288)
      t2291 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D
     #1, x4)
      t2293 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D
     #1, x4)
      t2305 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D
     #1, x4)
      t2319 = t9 * t15 * t2291
      t2349 = rrqg2qgh84J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D
     #1, x4)
      t2366 = (0.90D2 * t9 * 0.3141592653589793D1 * (t287 * t2291 - t229
     #3) + 0.180D3 * t9 * t57 * t2291) * t66 * t135 / 0.720D3 + (0.90D2 
     #* t9 * 0.3141592653589793D1 * (-t2305 + t304 * t2293 - t306 * t229
     #1 / 0.2D1) - 0.180D3 * t9 * t57 * (t304 * t2291 - t2293) - t2319) 
     #* t91 * t115 / 0.720D3 + (0.90D2 * t9 * 0.3141592653589793D1 * (-t
     #229 * t2291 / 0.2D1 - t2305 + t228 * t2293) - 0.180D3 * t9 * t57 *
     # (-t2293 + t228 * t2291) - t2319) * t66 * t91 / 0.1440D4 - (t9 * t
     #15 * (t2293 - t254 * t2291) + 0.90D2 * t9 * 0.3141592653589793D1 *
     # (t259 * t2293 / 0.2D1 - t263 * t2291 / 0.6D1 - t254 * t2305 + t23
     #49) + t9 * t54 * t2291 - 0.180D3 * t9 * t57 * (t2305 + t259 * t229
     #1 / 0.2D1 - t254 * t2293)) * t91 / 0.1440D4
      t2367 = FJET(XB1, XB2, s, 0.0D0, t223, 0.0D0, -t221, 0.0D0, t2366)
      t2369 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, t340, 0.0D0, 0.10
     #D1, x4)
      t2372 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, t340, 0.0D0, 0.10
     #D1, x4)
      t2373 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, t340, 0.0D0, 0.10
     #D1, x4)
      t2385 = t9 * t15 * t2369
      t2393 = rrqg2qgh84J4(s, XB1, XB2, z, lh, wd, nf, t340, 0.0D0, 0.10
     #D1, x4)
      t2441 = (0.90D2 * t9 * 0.3141592653589793D1 * (-t403 * t2369 / 0.2
     #D1 - t2372 + t399 * t2373) - 0.180D3 * t9 * t57 * (-t2373 + t399 *
     # t2369) - t2385) * t66 * t115 / 0.720D3 + (t9 * t15 * (t427 * t236
     #9 - t2373) + 0.90D2 * t9 * 0.3141592653589793D1 * (-t2393 + t436 *
     # t2369 / 0.6D1 + t427 * t2372 - t432 * t2373 / 0.2D1) - t9 * t54 *
     # t2369 - 0.180D3 * t9 * t57 * (-t432 * t2369 / 0.2D1 + t427 * t237
     #3 - t2372)) * t115 / 0.720D3 + (0.90D2 * t9 * 0.3141592653589793D1
     # * (t458 * t2369 - t2373) + 0.180D3 * t9 * t57 * t2369) * t66 * t1
     #35 / 0.720D3 + (0.90D2 * t9 * 0.3141592653589793D1 * (-t473 * t236
     #9 / 0.2D1 + t472 * t2373 - t2372) - 0.180D3 * t9 * t57 * (t472 * t
     #2369 - t2373) - t2385) * t91 * t115 / 0.720D3
      t2442 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t325, -t393, 0.0D0, t2441)
      t2444 = t1597 * t1596 + t1749 * t1748 + t1847 * t1843 * t1850 / 0.
     #720D3 + t1871 * t1867 * t1850 / 0.720D3 + t1893 * t1889 * t1850 / 
     #0.720D3 + t1915 * t1911 * t1850 / 0.720D3 + t1952 * t1951 + t1987 
     #* t1986 + t2022 * t2021 + t2097 * t2096 + t2132 * t2131 + t2173 * 
     #t2172 + t2214 * t2213 + t2289 * t2288 + t2367 * t2366 + t2442 * t2
     #441
      rrqg2qght8s2e1 = t1543 + t2444

      end function



      doubleprecision function rrqg2qght8s2e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = x2 * x3
      t4 = 0.3D1 * t3
      t5 = x2 ** 2
      t6 = t5 * x3
      t7 = x4 * 0.3141592653589793D1
      t8 = cos(t7)
      t9 = -0.1D1 + x3
      t11 = Sqrt(-t3 * t9)
      t12 = t8 * t11
      t13 = 0.2D1 * t12
      t15 = 0.2D1 * t12 * x2
      t17 = -0.1D1 + t3
      t18 = 0.1D1 / t17
      t20 = t2 * (-x2 - x3 + t4 - t6 - t13 + t15) * t18
      t21 = -0.1D1 + x2
      t25 = t2 * t21 * (-t3 - 0.1D1 + x3 + t13) * t18
      t26 = 0.1D1 / t1
      t27 = s ** 2
      t28 = 0.1D1 / t27
      t29 = t26 * t28
      t30 = x2 * z
      t31 = 0.1D1 - x2 + t30
      t32 = t9 * t18
      t33 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t32, x4
     #)
      t35 = t1 ** 2
      t36 = t35 ** 2
      t37 = Sin(t7)
      t38 = t37 ** 2
      t39 = t36 * t38
      t41 = z ** 2
      t42 = 0.1D1 / t41
      t43 = t21 ** 2
      t45 = t17 ** 2
      t51 = log(-0.4D1 * t3 * t39 * t42 * t43 * t9 / t45)
      t52 = t51 * t31
      t53 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t32, x4
     #)
      t57 = t6 * z
      t58 = t3 * z
      t59 = 0.2D1 * t3
      t63 = 0.1D1 / (t6 - t57 - t30 + t58 - t15 + x2 - t59 + 0.2D1 * t12
     # * t30 - 0.1D1 + t13)
      t67 = t29 * lh
      t68 = 0.3141592653589793D1 * t31
      t69 = t53 * t63
      t74 = 0.1D1 / x3
      t76 = 0.1D1 / x2
      t79 = t29 * t68
      t81 = 0.1D1 / x1
      t82 = t74 * t76 * t81
      t86 = (-0.90D2 * t29 * 0.3141592653589793D1 * (t31 * t33 - t52 * t
     #53) * t63 + 0.180D3 * t67 * t68 * t69) * t74 * t76 / 0.1440D4 - t7
     #9 * t69 * t82 / 0.8D1
      t87 = FJET(XB1, XB2, s, 0.0D0, t20, 0.0D0, -t25, 0.0D0, t86)
      t89 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t32, x4
     #)
      t91 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t32, x4
     #)
      t98 = t91 * t63
      t109 = (-0.90D2 * t29 * 0.3141592653589793D1 * (t31 * t89 - t52 * 
     #t91) * t63 + 0.180D3 * t67 * t68 * t98) * t74 * t76 / 0.1440D4 - t
     #79 * t98 * t82 / 0.8D1
      t110 = FJET(XB1, XB2, s, -t25, 0.0D0, t20, 0.0D0, 0.0D0, t109)
      t112 = -0.1D1 + x1
      t113 = t2 * t112
      t114 = x3 * x1
      t115 = x1 * z
      t116 = t114 * z
      t117 = 0.1D1 - x1 + t115
      t121 = Sqrt(-x3 * t117 * x2 * t9)
      t122 = t8 * t121
      t123 = 0.2D1 * t122
      t126 = 0.1D1 / t117
      t129 = t113 * t21 * (-t3 - 0.1D1 + x3 + x1 - t114 - t115 + t116 + 
     #t123) * t126 * t18
      t130 = t9 * s
      t131 = t1 * x1
      t133 = t130 * t131 * t18
      t135 = 0.2D1 * t122 * x2
      t136 = t3 * x1
      t138 = t6 * x1
      t139 = t3 * t115
      t141 = t6 * t115
      t142 = t135 - t116 - 0.2D1 * t136 + t138 + t4 + 0.2D1 * t139 - t14
     #1 - x2 - x3 + t114 - t6 - t123
      t145 = t113 * t142 * t126 * t18
      t146 = t2 * x1
      t149 = t146 * x3 * t21 * t18
      t154 = s * t35 * x2 * x1 * t112 * t126
      t159 = x2 * x1
      t160 = t159 * z
      t162 = x1 ** 2
      t163 = x3 * t162
      t166 = x2 * t162
      t174 = 0.1D1 + t135 - 0.3D1 * t136 + t138 + 0.2D1 * t122 * x1 - 0.
     #3D1 * t160 + t163 * x2 + t159 * t41 + 0.2D1 * t166 * z - t166 * t4
     #1 + t59 - t6 + t30 + 0.2D1 * t115 + 0.2D1 * t122 * t160 - 0.2D1 * 
     #x1
      t192 = t57 - t58 + 0.4D1 * t139 - t141 - 0.2D1 * t122 * t159 - 0.2
     #D1 * t122 * t115 - 0.2D1 * t122 * t30 - x3 * t41 * t159 - 0.2D1 * 
     #t163 * t30 + t163 * t41 * x2 + t162 - t123 + 0.2D1 * t159 - 0.2D1 
     #* t162 * z + t162 * t41 - t166 - x2
      t194 = 0.1D1 / (t174 + t192)
      t196 = t29 * 0.3141592653589793D1 * t117 * t194
      t197 = -t112
      t198 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, t197, x2, t32, x4)
      t199 = -0.1D1 + x2 + x1 - t30 - t159 + t160 - t115
      t204 = FJET(XB1, XB2, s, t129, t133, -t145, t149, -t154, t196 * t1
     #98 * t199 * t82 / 0.8D1)
      t207 = t28 * 0.3141592653589793D1 * t117
      t214 = t21 * s
      t215 = t1 * t112
      t216 = t214 * t215
      t219 = t2 * t112 * x2 * t126
      t220 = t29 * 0.3141592653589793D1
      t221 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, t197, x2, 0.10D1, 
     #x4)
      t223 = t76 * t81
      t227 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, t197, x2, 0.10D1, 
     #x4)
      t229 = t42 * t126
      t230 = t112 ** 2
      t235 = log(0.4D1 * t166 * t39 * t229 * t230 * t43)
      t241 = 0.3141592653589793D1 * lh
      t249 = t220 * t221 * t74 * t223 / 0.8D1 + (0.90D2 * t29 * 0.314159
     #2653589793D1 * (t227 - t235 * t221) - 0.180D3 * t29 * t241 * t221)
     # * t76 * t81 / 0.720D3
      t250 = FJET(XB1, XB2, s, t216, t146, -t219, 0.0D0, -t154, t249)
      t252 = t214 * t1
      t254 = x2 * s * t1
      t255 = t3 * t36
      t256 = t38 * t42
      t257 = t256 * t43
      t260 = log(0.4D1 * t255 * t257)
      t261 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t263 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t270 = 0.180D3 * t29 * t241 * t261
      t275 = x2 * t36
      t278 = log(0.4D1 * t275 * t257)
      t280 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t281 = t278 ** 2
      t293 = lh ** 2
      t295 = 0.3141592653589793D1 ** 2
      t297 = 0.180D3 * t293 - 0.30D2 * t295
      t298 = t297 * 0.3141592653589793D1
      t308 = t166 * t36
      t311 = log(0.4D1 * t308 * t257)
      t321 = (0.90D2 * t29 * 0.3141592653589793D1 * (t260 * t261 - t263)
     # + t270) * t74 * t76 / 0.1440D4 - (0.90D2 * t29 * 0.31415926535897
     #93D1 * (-t278 * t263 + t280 + t281 * t261 / 0.2D1) - 0.180D3 * t29
     # * t241 * (t263 - t278 * t261) + t29 * t298 * t261) * t76 / 0.1440
     #D4 - t220 * t261 * t74 * t223 / 0.8D1 + (0.90D2 * t29 * 0.31415926
     #53589793D1 * (t311 * t261 - t263) + t270) * t76 * t81 / 0.720D3
      t322 = FJET(XB1, XB2, s, -t252, 0.0D0, t254, 0.0D0, 0.0D0, t321)
      t324 = t2 * t9
      t325 = t2 * x3
      t327 = t256 * t9
      t330 = log(-0.4D1 * x3 * t36 * t327)
      t331 = t330 ** 2
      t332 = -t9
      t333 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t33
     #2, x4)
      t336 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t33
     #2, x4)
      t337 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t33
     #2, x4)
      t355 = log(-0.4D1 * t255 * t327)
      t363 = 0.180D3 * t29 * t241 * t333
      t371 = log(-0.4D1 * t163 * t36 * t327)
      t385 = (0.90D2 * t29 * 0.3141592653589793D1 * (-t331 * t333 / 0.2D
     #1 - t336 + t330 * t337) - 0.180D3 * t29 * t241 * (-t337 + t330 * t
     #333) - t29 * t298 * t333) * t74 / 0.1440D4 + (0.90D2 * t29 * 0.314
     #1592653589793D1 * (t355 * t333 - t337) + t363) * t74 * t76 / 0.144
     #0D4 + (0.90D2 * t29 * 0.3141592653589793D1 * (-t337 + t371 * t333)
     # + t363) * t74 * t81 / 0.720D3 - t220 * t333 * t74 * t223 / 0.8D1
      t386 = FJET(XB1, XB2, s, 0.0D0, -t324, 0.0D0, t325, 0.0D0, t385)
      t388 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t33
     #2, x4)
      t390 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t33
     #2, x4)
      t393 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t33
     #2, x4)
      t415 = 0.180D3 * t29 * t241 * t390
      t433 = (0.90D2 * t29 * 0.3141592653589793D1 * (t330 * t388 - t331 
     #* t390 / 0.2D1 - t393) - 0.180D3 * t29 * t241 * (t330 * t390 - t38
     #8) - t29 * t298 * t390) * t74 / 0.1440D4 + (0.90D2 * t29 * 0.31415
     #92653589793D1 * (t355 * t390 - t388) + t415) * t74 * t76 / 0.1440D
     #4 + (0.90D2 * t29 * 0.3141592653589793D1 * (t371 * t390 - t388) + 
     #t415) * t74 * t81 / 0.720D3 - t220 * t390 * t74 * t223 / 0.8D1
      t434 = FJET(XB1, XB2, s, t325, 0.0D0, -t324, 0.0D0, 0.0D0, t433)
      t436 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t438 = t42 * t36
      t441 = log(0.4D1 * x3 * t38 * t438)
      t442 = t441 ** 2
      t443 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t446 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t458 = t29 * t298 * t443
      t462 = t256 * t36
      t464 = log(0.4D1 * t462)
      t465 = t464 * t26
      t466 = t28 * lh
      t469 = t464 ** 2
      t470 = t469 * t26
      t475 = (0.180D3 * t465 * t466 + 0.45D2 * t470 * t28 + t29 * t297) 
     #* 0.3141592653589793D1
      t478 = rrqg2qgh84J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t497 = (-0.90D2 * t470 * t466 + t29 * (0.60D2 * lh * t295 - 0.2884
     #936567583026D3 - 0.120D3 * t293 * lh) - 0.15D2 * t469 * t464 * t26
     # * t28 - t465 * t28 * t297) * 0.3141592653589793D1
      t504 = (-0.180D3 * t67 - 0.90D2 * t465 * t28) * 0.3141592653589793
     #D1
      t509 = log(0.4D1 * t3 * t462)
      t517 = 0.180D3 * t29 * t241 * t443
      t524 = log(0.4D1 * t275 * t256)
      t526 = t524 ** 2
      t543 = log(0.4D1 * t163 * t462)
      t559 = log(0.4D1 * t166 * t462)
      t569 = t162 * t38
      t572 = log(0.4D1 * t569 * t438)
      t573 = t572 ** 2
      t589 = (0.90D2 * t29 * 0.3141592653589793D1 * (t436 + t442 * t443 
     #/ 0.2D1 - t441 * t446) - 0.180D3 * t29 * t241 * (t446 - t441 * t44
     #3) + t458) * t74 / 0.1440D4 + t475 * t446 / 0.1440D4 + t29 * 0.314
     #1592653589793D1 * t478 / 0.16D2 + t497 * t443 / 0.1440D4 + t504 * 
     #t436 / 0.1440D4 + (0.90D2 * t29 * 0.3141592653589793D1 * (t446 - t
     #509 * t443) - t517) * t74 * t76 / 0.1440D4 - (0.90D2 * t29 * 0.314
     #1592653589793D1 * (-t436 + t524 * t446 - t526 * t443 / 0.2D1) - 0.
     #180D3 * t29 * t241 * (-t446 + t524 * t443) - t458) * t76 / 0.1440D
     #4 + (0.90D2 * t29 * 0.3141592653589793D1 * (t446 - t543 * t443) - 
     #t517) * t74 * t81 / 0.720D3 + t220 * t443 * t74 * t223 / 0.8D1 + (
     #0.90D2 * t29 * 0.3141592653589793D1 * (t446 - t559 * t443) - t517)
     # * t76 * t81 / 0.720D3 + (0.90D2 * t29 * 0.3141592653589793D1 * (t
     #436 + t573 * t443 / 0.2D1 - t572 * t446) - 0.180D3 * t29 * t241 * 
     #(t446 - t572 * t443) + t458) * t81 / 0.720D3
      t590 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t589)
      t592 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, t197, 0.0D0, 0.10D
     #1, x4)
      t594 = t126 * t230
      t598 = log(0.4D1 * t163 * t38 * t438 * t594)
      t599 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, t197, 0.0D0, 0.10D
     #1, x4)
      t607 = 0.180D3 * t29 * t241 * t599
      t619 = log(0.4D1 * t308 * t256 * t594)
      t634 = log(0.4D1 * t569 * t42 * t36 * t126 * t230)
      t635 = t634 ** 2
      t639 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, t197, 0.0D0, 0.10D
     #1, x4)
      t654 = (0.90D2 * t29 * 0.3141592653589793D1 * (-t592 + t598 * t599
     #) + t607) * t74 * t81 / 0.720D3 - t220 * t599 * t74 * t223 / 0.8D1
     # + (0.90D2 * t29 * 0.3141592653589793D1 * (t619 * t599 - t592) + t
     #607) * t76 * t81 / 0.720D3 + (0.90D2 * t29 * 0.3141592653589793D1 
     #* (-t635 * t599 / 0.2D1 + t634 * t592 - t639) - 0.180D3 * t29 * t2
     #41 * (t634 * t599 - t592) - t29 * t298 * t599) * t81 / 0.720D3
      t655 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t146, -t113, 0.0D0, t654)
      t657 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, t197, x2, 0.10D1, 
     #x4)
      t662 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, t197, x2, 0.10D1, 
     #x4)
      t675 = t220 * t657 * t74 * t223 / 0.8D1 + (0.90D2 * t29 * 0.314159
     #2653589793D1 * (t662 - t235 * t657) - 0.180D3 * t29 * t241 * t657)
     # * t76 * t81 / 0.720D3
      t676 = FJET(XB1, XB2, s, t146, t216, 0.0D0, -t219, -t154, t675)
      t678 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t32, x
     #4)
      t680 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t32, x
     #4)
      t687 = t680 * t63
      t698 = (-0.90D2 * t29 * 0.3141592653589793D1 * (t31 * t678 - t52 *
     # t680) * t63 + 0.180D3 * t67 * t68 * t687) * t74 * t76 / 0.1440D4 
     #- t79 * t687 * t82 / 0.8D1
      t699 = FJET(XB1, XB2, s, t20, 0.0D0, -t25, 0.0D0, 0.0D0, t698)
      t701 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t32, x
     #4)
      t703 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t32, x
     #4)
      t710 = t703 * t63
      t721 = (-0.90D2 * t29 * 0.3141592653589793D1 * (t31 * t701 - t52 *
     # t703) * t63 + 0.180D3 * t67 * t68 * t710) * t74 * t76 / 0.1440D4 
     #- t79 * t710 * t82 / 0.8D1
      t722 = FJET(XB1, XB2, s, 0.0D0, -t25, 0.0D0, t20, 0.0D0, t721)
      t725 = x3 * s * t215
      t726 = t2 * t114
      t727 = t130 * t215
      t728 = t130 * t131
      t734 = log(-0.4D1 * t163 * t39 * t229 * t230 * t9)
      t735 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, t197, 0.0D0, t332,
     # x4)
      t737 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, t197, 0.0D0, t332,
     # x4)
      t753 = (0.90D2 * t29 * 0.3141592653589793D1 * (-t734 * t735 + t737
     #) - 0.180D3 * t29 * t241 * t735) * t74 * t81 / 0.720D3 + t220 * t7
     #35 * t74 * t223 / 0.8D1
      t754 = FJET(XB1, XB2, s, -t725, t726, t727, -t728, 0.0D0, t753)
      t756 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, t197, x2, t32, x4)
      t761 = FJET(XB1, XB2, s, -t145, t149, t129, t133, -t154, t196 * t7
     #56 * t199 * t82 / 0.8D1)
      t769 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t770 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t778 = 0.180D3 * t29 * t241 * t770
      t785 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t814 = (0.90D2 * t29 * 0.3141592653589793D1 * (-t769 + t260 * t770
     #) + t778) * t74 * t76 / 0.1440D4 - (0.90D2 * t29 * 0.3141592653589
     #793D1 * (t281 * t770 / 0.2D1 + t785 - t278 * t769) - 0.180D3 * t29
     # * t241 * (t769 - t278 * t770) + t29 * t298 * t770) * t76 / 0.1440
     #D4 - t220 * t770 * t74 * t223 / 0.8D1 + (0.90D2 * t29 * 0.31415926
     #53589793D1 * (t311 * t770 - t769) + t778) * t76 * t81 / 0.720D3
      t815 = FJET(XB1, XB2, s, 0.0D0, -t252, 0.0D0, t254, 0.0D0, t814)
      t817 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, t197, x2, 0.10D1, 
     #x4)
      t822 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, t197, x2, 0.10D1, 
     #x4)
      t835 = t220 * t817 * t74 * t223 / 0.8D1 + (0.90D2 * t29 * 0.314159
     #2653589793D1 * (t822 - t235 * t817) - 0.180D3 * t29 * t241 * t817)
     # * t76 * t81 / 0.720D3
      t836 = FJET(XB1, XB2, s, -t219, 0.0D0, t216, t146, -t154, t835)
      t838 = t87 * t86 + t110 * t109 + t204 * t26 * t207 * t194 * t198 *
     # t199 * t82 / 0.8D1 + t250 * t249 + t322 * t321 + t386 * t385 + t4
     #34 * t433 + t590 * t589 + t655 * t654 + t676 * t675 + t699 * t698 
     #+ t722 * t721 + t754 * t753 + t761 * t26 * t207 * t194 * t756 * t1
     #99 * t82 / 0.8D1 + t815 * t814 + t836 * t835
      t839 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t841 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t842 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t855 = t29 * t298 * t842
      t861 = rrqg2qgh82J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t876 = 0.180D3 * t29 * t241 * t842
      t933 = (0.90D2 * t29 * 0.3141592653589793D1 * (-t441 * t839 + t841
     # + t442 * t842 / 0.2D1) - 0.180D3 * t29 * t241 * (t839 - t441 * t8
     #42) + t855) * t74 / 0.1440D4 + t475 * t839 / 0.1440D4 + t29 * 0.31
     #41592653589793D1 * t861 / 0.16D2 + t497 * t842 / 0.1440D4 + t504 *
     # t841 / 0.1440D4 + (0.90D2 * t29 * 0.3141592653589793D1 * (t839 - 
     #t509 * t842) - t876) * t74 * t76 / 0.1440D4 - (0.90D2 * t29 * 0.31
     #41592653589793D1 * (-t841 + t524 * t839 - t526 * t842 / 0.2D1) - 0
     #.180D3 * t29 * t241 * (-t839 + t524 * t842) - t855) * t76 / 0.1440
     #D4 + (0.90D2 * t29 * 0.3141592653589793D1 * (t839 - t543 * t842) -
     # t876) * t74 * t81 / 0.720D3 + t220 * t842 * t74 * t223 / 0.8D1 + 
     #(0.90D2 * t29 * 0.3141592653589793D1 * (t839 - t559 * t842) - t876
     #) * t76 * t81 / 0.720D3 + (0.90D2 * t29 * 0.3141592653589793D1 * (
     #-t572 * t839 + t841 + t573 * t842 / 0.2D1) - 0.180D3 * t29 * t241 
     #* (t839 - t572 * t842) + t855) * t81 / 0.720D3
      t934 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t933)
      t936 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t33
     #2, x4)
      t939 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t33
     #2, x4)
      t941 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t33
     #2, x4)
      t963 = 0.180D3 * t29 * t241 * t936
      t981 = (0.90D2 * t29 * 0.3141592653589793D1 * (-t331 * t936 / 0.2D
     #1 + t330 * t939 - t941) - 0.180D3 * t29 * t241 * (t330 * t936 - t9
     #39) - t29 * t298 * t936) * t74 / 0.1440D4 + (0.90D2 * t29 * 0.3141
     #592653589793D1 * (t355 * t936 - t939) + t963) * t74 * t76 / 0.1440
     #D4 + (0.90D2 * t29 * 0.3141592653589793D1 * (t371 * t936 - t939) +
     # t963) * t74 * t81 / 0.720D3 - t220 * t936 * t74 * t223 / 0.8D1
      t982 = FJET(XB1, XB2, s, 0.0D0, t325, 0.0D0, -t324, 0.0D0, t981)
      t984 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, t197, x2, 0.10D1, 
     #x4)
      t990 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, t197, x2, 0.10D1, 
     #x4)
      t1002 = t220 * t984 * t74 * t223 / 0.8D1 + (0.90D2 * t29 * 0.31415
     #92653589793D1 * (-t235 * t984 + t990) - 0.180D3 * t29 * t241 * t98
     #4) * t76 * t81 / 0.720D3
      t1003 = FJET(XB1, XB2, s, 0.0D0, -t219, t146, t216, -t154, t1002)
      t1005 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, t197, 0.0D0, t332
     #, x4)
      t1007 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, t197, 0.0D0, t332
     #, x4)
      t1023 = (0.90D2 * t29 * 0.3141592653589793D1 * (-t734 * t1005 + t1
     #007) - 0.180D3 * t29 * t241 * t1005) * t74 * t81 / 0.720D3 + t220 
     #* t1005 * t74 * t223 / 0.8D1
      t1024 = FJET(XB1, XB2, s, t727, -t728, -t725, t726, 0.0D0, t1023)
      t1026 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D
     #1, x4)
      t1028 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D
     #1, x4)
      t1035 = 0.180D3 * t29 * t241 * t1026
      t1041 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D
     #1, x4)
      t1071 = (0.90D2 * t29 * 0.3141592653589793D1 * (t260 * t1026 - t10
     #28) + t1035) * t74 * t76 / 0.1440D4 - (0.90D2 * t29 * 0.3141592653
     #589793D1 * (-t278 * t1028 + t1041 + t281 * t1026 / 0.2D1) - 0.180D
     #3 * t29 * t241 * (t1028 - t278 * t1026) + t29 * t298 * t1026) * t7
     #6 / 0.1440D4 - t220 * t1026 * t74 * t223 / 0.8D1 + (0.90D2 * t29 *
     # 0.3141592653589793D1 * (t311 * t1026 - t1028) + t1035) * t76 * t8
     #1 / 0.720D3
      t1072 = FJET(XB1, XB2, s, t254, 0.0D0, -t252, 0.0D0, 0.0D0, t1071)
      t1074 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, t197, 0.0D0, 0.10
     #D1, x4)
      t1076 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, t197, 0.0D0, 0.10
     #D1, x4)
      t1083 = 0.180D3 * t29 * t241 * t1074
      t1101 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, t197, 0.0D0, 0.10
     #D1, x4)
      t1119 = (0.90D2 * t29 * 0.3141592653589793D1 * (t598 * t1074 - t10
     #76) + t1083) * t74 * t81 / 0.720D3 - t220 * t1074 * t74 * t223 / 0
     #.8D1 + (0.90D2 * t29 * 0.3141592653589793D1 * (-t1076 + t619 * t10
     #74) + t1083) * t76 * t81 / 0.720D3 + (0.90D2 * t29 * 0.31415926535
     #89793D1 * (-t1101 - t635 * t1074 / 0.2D1 + t634 * t1076) - 0.180D3
     # * t29 * t241 * (-t1076 + t634 * t1074) - t29 * t298 * t1074) * t8
     #1 / 0.720D3
      t1120 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t113, t146, 0.0D0, t1119)
      t1122 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D
     #1, x4)
      t1123 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D
     #1, x4)
      t1131 = 0.180D3 * t29 * t241 * t1123
      t1136 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D
     #1, x4)
      t1167 = (0.90D2 * t29 * 0.3141592653589793D1 * (-t1122 + t260 * t1
     #123) + t1131) * t74 * t76 / 0.1440D4 - (0.90D2 * t29 * 0.314159265
     #3589793D1 * (t1136 + t281 * t1123 / 0.2D1 - t278 * t1122) - 0.180D
     #3 * t29 * t241 * (t1122 - t278 * t1123) + t29 * t298 * t1123) * t7
     #6 / 0.1440D4 - t220 * t1123 * t74 * t223 / 0.8D1 + (0.90D2 * t29 *
     # 0.3141592653589793D1 * (t311 * t1123 - t1122) + t1131) * t76 * t8
     #1 / 0.720D3
      t1168 = FJET(XB1, XB2, s, 0.0D0, t254, 0.0D0, -t252, 0.0D0, t1167)
      t1170 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, t197, 0.0D0, 0.10
     #D1, x4)
      t1172 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, t197, 0.0D0, 0.10
     #D1, x4)
      t1179 = 0.180D3 * t29 * t241 * t1170
      t1198 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, t197, 0.0D0, 0.10
     #D1, x4)
      t1215 = (0.90D2 * t29 * 0.3141592653589793D1 * (t598 * t1170 - t11
     #72) + t1179) * t74 * t81 / 0.720D3 - t220 * t1170 * t74 * t223 / 0
     #.8D1 + (0.90D2 * t29 * 0.3141592653589793D1 * (-t1172 + t619 * t11
     #70) + t1179) * t76 * t81 / 0.720D3 + (0.90D2 * t29 * 0.31415926535
     #89793D1 * (t634 * t1172 - t1198 - t635 * t1170 / 0.2D1) - 0.180D3 
     #* t29 * t241 * (-t1172 + t634 * t1170) - t29 * t298 * t1170) * t81
     # / 0.720D3
      t1216 = FJET(XB1, XB2, s, -t113, t146, 0.0D0, 0.0D0, 0.0D0, t1215)
      t1218 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t1220 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t1221 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t1234 = t29 * t298 * t1221
      t1240 = rrqg2qgh83J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t1255 = 0.180D3 * t29 * t241 * t1221
      t1312 = (0.90D2 * t29 * 0.3141592653589793D1 * (-t441 * t1218 + t1
     #220 + t442 * t1221 / 0.2D1) - 0.180D3 * t29 * t241 * (-t441 * t122
     #1 + t1218) + t1234) * t74 / 0.1440D4 + t475 * t1218 / 0.1440D4 + t
     #29 * 0.3141592653589793D1 * t1240 / 0.16D2 + t497 * t1221 / 0.1440
     #D4 + t504 * t1220 / 0.1440D4 + (0.90D2 * t29 * 0.3141592653589793D
     #1 * (-t509 * t1221 + t1218) - t1255) * t74 * t76 / 0.1440D4 - (0.9
     #0D2 * t29 * 0.3141592653589793D1 * (t524 * t1218 - t1220 - t526 * 
     #t1221 / 0.2D1) - 0.180D3 * t29 * t241 * (-t1218 + t524 * t1221) - 
     #t1234) * t76 / 0.1440D4 + (0.90D2 * t29 * 0.3141592653589793D1 * (
     #-t543 * t1221 + t1218) - t1255) * t74 * t81 / 0.720D3 + t220 * t12
     #21 * t74 * t223 / 0.8D1 + (0.90D2 * t29 * 0.3141592653589793D1 * (
     #t1218 - t559 * t1221) - t1255) * t76 * t81 / 0.720D3 + (0.90D2 * t
     #29 * 0.3141592653589793D1 * (t1220 + t573 * t1221 / 0.2D1 - t572 *
     # t1218) - 0.180D3 * t29 * t241 * (t1218 - t572 * t1221) + t1234) *
     # t81 / 0.720D3
      t1313 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t1312)
      t1315 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, t197, 0.0D0, t332
     #, x4)
      t1317 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, t197, 0.0D0, t332
     #, x4)
      t1333 = (0.90D2 * t29 * 0.3141592653589793D1 * (-t734 * t1315 + t1
     #317) - 0.180D3 * t29 * t241 * t1315) * t74 * t81 / 0.720D3 + t220 
     #* t1315 * t74 * t223 / 0.8D1
      t1334 = FJET(XB1, XB2, s, -t728, t727, t726, -t725, 0.0D0, t1333)
      t1336 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t3
     #32, x4)
      t1338 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t3
     #32, x4)
      t1341 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t3
     #32, x4)
      t1363 = 0.180D3 * t29 * t241 * t1338
      t1381 = (0.90D2 * t29 * 0.3141592653589793D1 * (t330 * t1336 - t33
     #1 * t1338 / 0.2D1 - t1341) - 0.180D3 * t29 * t241 * (t330 * t1338 
     #- t1336) - t29 * t298 * t1338) * t74 / 0.1440D4 + (0.90D2 * t29 * 
     #0.3141592653589793D1 * (t355 * t1338 - t1336) + t1363) * t74 * t76
     # / 0.1440D4 + (0.90D2 * t29 * 0.3141592653589793D1 * (-t1336 + t37
     #1 * t1338) + t1363) * t74 * t81 / 0.720D3 - t220 * t1338 * t74 * t
     #223 / 0.8D1
      t1382 = FJET(XB1, XB2, s, -t324, 0.0D0, t325, 0.0D0, 0.0D0, t1381)
      t1384 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, t197, 0.0D0, 0.10
     #D1, x4)
      t1386 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, t197, 0.0D0, 0.10
     #D1, x4)
      t1393 = 0.180D3 * t29 * t241 * t1384
      t1411 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, t197, 0.0D0, 0.10
     #D1, x4)
      t1429 = (0.90D2 * t29 * 0.3141592653589793D1 * (t598 * t1384 - t13
     #86) + t1393) * t74 * t81 / 0.720D3 - t220 * t1384 * t74 * t223 / 0
     #.8D1 + (0.90D2 * t29 * 0.3141592653589793D1 * (t619 * t1384 - t138
     #6) + t1393) * t76 * t81 / 0.720D3 + (0.90D2 * t29 * 0.314159265358
     #9793D1 * (-t1411 - t635 * t1384 / 0.2D1 + t634 * t1386) - 0.180D3 
     #* t29 * t241 * (-t1386 + t634 * t1384) - t29 * t298 * t1384) * t81
     # / 0.720D3
      t1430 = FJET(XB1, XB2, s, t146, -t113, 0.0D0, 0.0D0, 0.0D0, t1429)
      t1432 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, t197, 0.0D0, t332
     #, x4)
      t1433 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, t197, 0.0D0, t332
     #, x4)
      t1450 = (0.90D2 * t29 * 0.3141592653589793D1 * (t1432 - t734 * t14
     #33) - 0.180D3 * t29 * t241 * t1433) * t74 * t81 / 0.720D3 + t220 *
     # t1433 * t74 * t223 / 0.8D1
      t1451 = FJET(XB1, XB2, s, t726, -t725, -t728, t727, 0.0D0, t1450)
      t1453 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t1454 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t1456 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t1469 = t29 * t298 * t1456
      t1475 = rrqg2qgh81J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #10D1, x4)
      t1488 = 0.180D3 * t29 * t241 * t1456
      t1547 = (0.90D2 * t29 * 0.3141592653589793D1 * (t1453 - t441 * t14
     #54 + t442 * t1456 / 0.2D1) - 0.180D3 * t29 * t241 * (t1454 - t441 
     #* t1456) + t1469) * t74 / 0.1440D4 + t504 * t1453 / 0.1440D4 + t29
     # * 0.3141592653589793D1 * t1475 / 0.16D2 + t497 * t1456 / 0.1440D4
     # + (0.90D2 * t29 * 0.3141592653589793D1 * (-t509 * t1456 + t1454) 
     #- t1488) * t74 * t76 / 0.1440D4 - (0.90D2 * t29 * 0.31415926535897
     #93D1 * (-t1453 - t526 * t1456 / 0.2D1 + t524 * t1454) - 0.180D3 * 
     #t29 * t241 * (-t1454 + t524 * t1456) - t1469) * t76 / 0.1440D4 + t
     #475 * t1454 / 0.1440D4 + (0.90D2 * t29 * 0.3141592653589793D1 * (t
     #1454 - t543 * t1456) - t1488) * t74 * t81 / 0.720D3 + t220 * t1456
     # * t74 * t223 / 0.8D1 + (0.90D2 * t29 * 0.3141592653589793D1 * (-t
     #559 * t1456 + t1454) - t1488) * t76 * t81 / 0.720D3 + (0.90D2 * t2
     #9 * 0.3141592653589793D1 * (-t572 * t1454 + t1453 + t573 * t1456 /
     # 0.2D1) - 0.180D3 * t29 * t241 * (-t572 * t1456 + t1454) + t1469) 
     #* t81 / 0.720D3
      t1548 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t1547)
      t1550 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, t197, x2, t32, x4
     #)
      t1555 = FJET(XB1, XB2, s, t149, -t145, t133, t129, -t154, t196 * t
     #1550 * t199 * t82 / 0.8D1)
      t1563 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, t197, x2, t32, x4
     #)
      t1568 = FJET(XB1, XB2, s, t133, t129, t149, -t145, -t154, t196 * t
     #1563 * t199 * t82 / 0.8D1)
      t1576 = t934 * t933 + t982 * t981 + t1003 * t1002 + t1024 * t1023 
     #+ t1072 * t1071 + t1120 * t1119 + t1168 * t1167 + t1216 * t1215 + 
     #t1313 * t1312 + t1334 * t1333 + t1382 * t1381 + t1430 * t1429 + t1
     #451 * t1450 + t1548 * t1547 + t1555 * t26 * t207 * t194 * t1550 * 
     #t199 * t82 / 0.8D1 + t1568 * t26 * t207 * t194 * t1563 * t199 * t8
     #2 / 0.8D1
      rrqg2qght8s2e0 = t838 + t1576

      end function



      doubleprecision function rrqg2qght8s2em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6
      t1 = -0.1D1 + x3
      t2 = s * t1
      t3 = -0.1D1 + z
      t5 = t2 * t3 * x1
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t8 = t2 * t7
      t9 = s * t3
      t11 = t9 * x1 * x3
      t13 = x3 * s * t7
      t14 = 0.1D1 / t3
      t15 = s ** 2
      t16 = 0.1D1 / t15
      t17 = t14 * t16
      t18 = t17 * 0.3141592653589793D1
      t19 = -t6
      t20 = -t1
      t21 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, t19, 0.0D0, t20, x4
     #)
      t22 = 0.1D1 / x3
      t24 = 0.1D1 / x1
      t28 = FJET(XB1, XB2, s, -t5, t8, t11, -t13, 0.0D0, t18 * t21 * t22
     # * t24 / 0.8D1)
      t32 = t22 * t24
      t36 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, t19, 0.0D0, t20, x4
     #)
      t41 = FJET(XB1, XB2, s, t11, -t13, -t5, t8, 0.0D0, t18 * t36 * t22
     # * t24 / 0.8D1)
      t48 = t9 * x1
      t49 = -0.1D1 + x2
      t50 = t49 * s
      t51 = t50 * t7
      t55 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t57 = t9 * t6 * x2 * t55
      t58 = t3 ** 2
      t63 = s * t58 * x2 * x1 * t6 * t55
      t64 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, t19, x2, 0.10D1, x4
     #)
      t65 = 0.1D1 / x2
      t70 = FJET(XB1, XB2, s, t48, t51, 0.0D0, -t57, -t63, t18 * t64 * t
     #65 * t24 / 0.8D1)
      t74 = t65 * t24
      t78 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, t19, x2, 0.10D1, x4
     #)
      t83 = FJET(XB1, XB2, s, 0.0D0, -t57, t48, t51, -t63, t18 * t78 * t
     #65 * t24 / 0.8D1)
      t90 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, t19, 0.0D0, t20, x4
     #)
      t95 = FJET(XB1, XB2, s, t8, -t5, -t13, t11, 0.0D0, t18 * t90 * t22
     # * t24 / 0.8D1)
      t102 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, t19, x2, 0.10D1, x
     #4)
      t107 = FJET(XB1, XB2, s, t51, t48, -t57, 0.0D0, -t63, t18 * t102 *
     # t65 * t24 / 0.8D1)
      t114 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, t19, x2, 0.10D1, x
     #4)
      t119 = FJET(XB1, XB2, s, -t57, 0.0D0, t51, t48, -t63, t18 * t114 *
     # t65 * t24 / 0.8D1)
      t126 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, t19, 0.0D0, t20, x
     #4)
      t131 = FJET(XB1, XB2, s, -t13, t11, t8, -t5, 0.0D0, t18 * t126 * t
     #22 * t24 / 0.8D1)
      t138 = x2 * x3
      t139 = x4 * 0.3141592653589793D1
      t140 = cos(t139)
      t142 = Sqrt(-t138 * t1)
      t143 = t140 * t142
      t144 = 0.2D1 * t143
      t148 = 0.1D1 / (-0.1D1 + t138)
      t150 = t9 * t49 * (-t138 - 0.1D1 + x3 + t144) * t148
      t152 = x2 ** 2
      t153 = t152 * x3
      t155 = 0.2D1 * t143 * x2
      t158 = t9 * (-x2 - x3 + 0.3D1 * t138 - t153 - t144 + t155) * t148
      t159 = x2 * z
      t160 = 0.1D1 - x2 + t159
      t162 = t17 * 0.3141592653589793D1 * t160
      t163 = t1 * t148
      t164 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t163, 
     #x4)
      t171 = 0.1D1 / (t153 - t153 * z - t159 + t138 * z - t155 + x2 - 0.
     #2D1 * t138 + 0.2D1 * t143 * t159 - 0.1D1 + t144)
      t173 = t22 * t65
      t177 = FJET(XB1, XB2, s, 0.0D0, -t150, 0.0D0, t158, 0.0D0, -t162 *
     # t164 * t171 * t173 / 0.16D2)
      t179 = t16 * 0.3141592653589793D1
      t183 = t171 * t22 * t65
      t187 = t50 * t3
      t189 = x2 * s * t3
      t190 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t199 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t200 = t58 ** 2
      t201 = x2 * t200
      t202 = Sin(t139)
      t203 = t202 ** 2
      t204 = z ** 2
      t205 = 0.1D1 / t204
      t206 = t203 * t205
      t207 = t49 ** 2
      t211 = log(0.4D1 * t201 * t206 * t207)
      t217 = 0.3141592653589793D1 * lh
      t224 = -t18 * t190 * t65 * t24 / 0.8D1 - t18 * t190 * t22 * t65 / 
     #0.16D2 - (0.90D2 * t17 * 0.3141592653589793D1 * (t199 - t211 * t19
     #0) - 0.180D3 * t17 * t217 * t190) * t65 / 0.1440D4
      t225 = FJET(XB1, XB2, s, -t187, 0.0D0, t189, 0.0D0, 0.0D0, t224)
      t227 = t9 * t6
      t228 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, t19, 0.0D0, 0.10D1
     #, x4)
      t233 = x1 ** 2
      t234 = t233 * t203
      t237 = t6 ** 2
      t241 = log(0.4D1 * t234 * t205 * t200 * t55 * t237)
      t243 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, t19, 0.0D0, 0.10D1
     #, x4)
      t258 = -t18 * t228 * t65 * t24 / 0.8D1 + (0.90D2 * t17 * 0.3141592
     #653589793D1 * (t241 * t228 - t243) + 0.180D3 * t17 * t217 * t228) 
     #* t24 / 0.720D3 - t18 * t228 * t22 * t24 / 0.8D1
      t259 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t48, -t227, 0.0D0, t258)
      t261 = t9 * x3
      t262 = t9 * t1
      t267 = log(-0.4D1 * x3 * t200 * t206 * t1)
      t268 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t20
     #, x4)
      t270 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t20
     #, x4)
      t281 = t268 * t22
      t288 = (0.90D2 * t17 * 0.3141592653589793D1 * (t267 * t268 - t270)
     # + 0.180D3 * t17 * t217 * t268) * t22 / 0.1440D4 - t18 * t281 * t2
     #4 / 0.8D1 - t18 * t281 * t65 / 0.16D2
      t289 = FJET(XB1, XB2, s, t261, 0.0D0, -t262, 0.0D0, 0.0D0, t288)
      t291 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, t19, 0.0D0, 0.10D1
     #, x4)
      t296 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, t19, 0.0D0, 0.10D1
     #, x4)
      t312 = -t18 * t291 * t65 * t24 / 0.8D1 + (0.90D2 * t17 * 0.3141592
     #653589793D1 * (-t296 + t241 * t291) + 0.180D3 * t17 * t217 * t291)
     # * t24 / 0.720D3 - t18 * t291 * t22 * t24 / 0.8D1
      t313 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t227, t48, 0.0D0, t312)
      t315 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t163, 
     #x4)
      t320 = FJET(XB1, XB2, s, t158, 0.0D0, -t150, 0.0D0, 0.0D0, -t162 *
     # t315 * t171 * t173 / 0.16D2)
      t327 = t28 * t14 * t16 * 0.3141592653589793D1 * t21 * t32 / 0.8D1 
     #+ t41 * t14 * t16 * 0.3141592653589793D1 * t36 * t32 / 0.8D1 + t70
     # * t14 * t16 * 0.3141592653589793D1 * t64 * t74 / 0.8D1 + t83 * t1
     #4 * t16 * 0.3141592653589793D1 * t78 * t74 / 0.8D1 + t95 * t14 * t
     #16 * 0.3141592653589793D1 * t90 * t32 / 0.8D1 + t107 * t14 * t16 *
     # 0.3141592653589793D1 * t102 * t74 / 0.8D1 + t119 * t14 * t16 * 0.
     #3141592653589793D1 * t114 * t74 / 0.8D1 + t131 * t14 * t16 * 0.314
     #1592653589793D1 * t126 * t32 / 0.8D1 - t177 * t14 * t179 * t160 * 
     #t164 * t183 / 0.16D2 + t225 * t224 + t259 * t258 + t289 * t288 + t
     #313 * t312 - t320 * t14 * t179 * t160 * t315 * t183 / 0.16D2
      t328 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t163, 
     #x4)
      t333 = FJET(XB1, XB2, s, 0.0D0, t158, 0.0D0, -t150, 0.0D0, -t162 *
     # t328 * t171 * t173 / 0.16D2)
      t340 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, t19, 0.0D0, 0.10D1
     #, x4)
      t345 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, t19, 0.0D0, 0.10D1
     #, x4)
      t361 = -t18 * t340 * t65 * t24 / 0.8D1 + (0.90D2 * t17 * 0.3141592
     #653589793D1 * (-t345 + t241 * t340) + 0.180D3 * t17 * t217 * t340)
     # * t24 / 0.720D3 - t18 * t340 * t22 * t24 / 0.8D1
      t362 = FJET(XB1, XB2, s, -t227, t48, 0.0D0, 0.0D0, 0.0D0, t361)
      t364 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t20
     #, x4)
      t365 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t20
     #, x4)
      t377 = t365 * t22
      t384 = (0.90D2 * t17 * 0.3141592653589793D1 * (-t364 + t267 * t365
     #) + 0.180D3 * t17 * t217 * t365) * t22 / 0.1440D4 - t18 * t377 * t
     #24 / 0.8D1 - t18 * t377 * t65 / 0.16D2
      t385 = FJET(XB1, XB2, s, 0.0D0, -t262, 0.0D0, t261, 0.0D0, t384)
      t388 = t205 * t200
      t391 = log(0.4D1 * x3 * t203 * t388)
      t392 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t394 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t401 = 0.180D3 * t17 * t217 * t392
      t405 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t413 = log(0.4D1 * t206 * t200)
      t414 = t413 * t14
      t418 = (-0.180D3 * t17 * lh - 0.90D2 * t414 * t16) * 0.31415926535
     #89793D1
      t424 = t413 ** 2
      t428 = lh ** 2
      t430 = 0.3141592653589793D1 ** 2
      t435 = (0.180D3 * t414 * t16 * lh + 0.45D2 * t424 * t14 * t16 + t1
     #7 * (0.180D3 * t428 - 0.30D2 * t430)) * 0.3141592653589793D1
      t438 = t392 * t22
      t444 = log(0.4D1 * t201 * t206)
      t459 = log(0.4D1 * t234 * t388)
      t471 = (0.90D2 * t17 * 0.3141592653589793D1 * (-t391 * t392 + t394
     #) - t401) * t22 / 0.1440D4 + t17 * 0.3141592653589793D1 * t405 / 0
     #.16D2 + t418 * t394 / 0.1440D4 + t435 * t392 / 0.1440D4 + t18 * t4
     #38 * t65 / 0.16D2 - (0.90D2 * t17 * 0.3141592653589793D1 * (-t394 
     #+ t444 * t392) + t401) * t65 / 0.1440D4 + t18 * t392 * t65 * t24 /
     # 0.8D1 + (0.90D2 * t17 * 0.3141592653589793D1 * (t394 - t459 * t39
     #2) - t401) * t24 / 0.720D3 + t18 * t438 * t24 / 0.8D1
      t472 = FJET(XB1, XB2, s, 0.0D0, t9, 0.0D0, 0.0D0, 0.0D0, t471)
      t474 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, t19, 0.0D0, 0.10D1
     #, x4)
      t479 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, t19, 0.0D0, 0.10D1
     #, x4)
      t495 = -t18 * t474 * t65 * t24 / 0.8D1 + (0.90D2 * t17 * 0.3141592
     #653589793D1 * (-t479 + t241 * t474) + 0.180D3 * t17 * t217 * t474)
     # * t24 / 0.720D3 - t18 * t474 * t22 * t24 / 0.8D1
      t496 = FJET(XB1, XB2, s, t48, -t227, 0.0D0, 0.0D0, 0.0D0, t495)
      t498 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t503 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t519 = -t18 * t498 * t22 * t65 / 0.16D2 - (0.90D2 * t17 * 0.314159
     #2653589793D1 * (t503 - t211 * t498) - 0.180D3 * t17 * t217 * t498)
     # * t65 / 0.1440D4 - t18 * t498 * t65 * t24 / 0.8D1
      t520 = FJET(XB1, XB2, s, 0.0D0, -t187, 0.0D0, t189, 0.0D0, t519)
      t522 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t527 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t543 = -t18 * t522 * t22 * t65 / 0.16D2 - (0.90D2 * t17 * 0.314159
     #2653589793D1 * (t527 - t211 * t522) - 0.180D3 * t17 * t217 * t522)
     # * t65 / 0.1440D4 - t18 * t522 * t65 * t24 / 0.8D1
      t544 = FJET(XB1, XB2, s, 0.0D0, t189, 0.0D0, -t187, 0.0D0, t543)
      t546 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t547 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t555 = 0.180D3 * t17 * t217 * t547
      t559 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t567 = t547 * t22
      t594 = (0.90D2 * t17 * 0.3141592653589793D1 * (t546 - t391 * t547)
     # - t555) * t22 / 0.1440D4 + t17 * 0.3141592653589793D1 * t559 / 0.
     #16D2 + t418 * t546 / 0.1440D4 + t435 * t547 / 0.1440D4 + t18 * t56
     #7 * t65 / 0.16D2 - (0.90D2 * t17 * 0.3141592653589793D1 * (-t546 +
     # t444 * t547) + t555) * t65 / 0.1440D4 + t18 * t547 * t65 * t24 / 
     #0.8D1 + (0.90D2 * t17 * 0.3141592653589793D1 * (t546 - t459 * t547
     #) - t555) * t24 / 0.720D3 + t18 * t567 * t24 / 0.8D1
      t595 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t9, 0.0D0, t594)
      t597 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t20
     #, x4)
      t599 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t20
     #, x4)
      t610 = t597 * t22
      t617 = (0.90D2 * t17 * 0.3141592653589793D1 * (t267 * t597 - t599)
     # + 0.180D3 * t17 * t217 * t597) * t22 / 0.1440D4 - t18 * t610 * t2
     #4 / 0.8D1 - t18 * t610 * t65 / 0.16D2
      t618 = FJET(XB1, XB2, s, -t262, 0.0D0, t261, 0.0D0, 0.0D0, t617)
      t620 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t20
     #, x4)
      t622 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t20
     #, x4)
      t633 = t620 * t22
      t640 = (0.90D2 * t17 * 0.3141592653589793D1 * (t267 * t620 - t622)
     # + 0.180D3 * t17 * t217 * t620) * t22 / 0.1440D4 - t18 * t633 * t6
     #5 / 0.16D2 - t18 * t633 * t24 / 0.8D1
      t641 = FJET(XB1, XB2, s, 0.0D0, t261, 0.0D0, -t262, 0.0D0, t640)
      t643 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t652 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t664 = -t18 * t643 * t65 * t24 / 0.8D1 - t18 * t643 * t22 * t65 / 
     #0.16D2 - (0.90D2 * t17 * 0.3141592653589793D1 * (t652 - t211 * t64
     #3) - 0.180D3 * t17 * t217 * t643) * t65 / 0.1440D4
      t665 = FJET(XB1, XB2, s, t189, 0.0D0, -t187, 0.0D0, 0.0D0, t664)
      t667 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t668 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t676 = 0.180D3 * t17 * t217 * t668
      t692 = t668 * t22
      t711 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t715 = (0.90D2 * t17 * 0.3141592653589793D1 * (t667 - t391 * t668)
     # - t676) * t22 / 0.1440D4 + t18 * t668 * t65 * t24 / 0.8D1 + (0.90
     #D2 * t17 * 0.3141592653589793D1 * (-t459 * t668 + t667) - t676) * 
     #t24 / 0.720D3 + t18 * t692 * t24 / 0.8D1 + t418 * t667 / 0.1440D4 
     #+ t435 * t668 / 0.1440D4 + t18 * t692 * t65 / 0.16D2 - (0.90D2 * t
     #17 * 0.3141592653589793D1 * (-t667 + t444 * t668) + t676) * t65 / 
     #0.1440D4 + t17 * 0.3141592653589793D1 * t711 / 0.16D2
      t716 = FJET(XB1, XB2, s, t9, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t715)
      t718 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t163, 
     #x4)
      t723 = FJET(XB1, XB2, s, -t150, 0.0D0, t158, 0.0D0, 0.0D0, -t162 *
     # t718 * t171 * t173 / 0.16D2)
      t730 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t731 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t739 = 0.180D3 * t17 * t217 * t731
      t743 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t751 = t731 * t22
      t778 = (0.90D2 * t17 * 0.3141592653589793D1 * (t730 - t391 * t731)
     # - t739) * t22 / 0.1440D4 + t17 * 0.3141592653589793D1 * t743 / 0.
     #16D2 + t418 * t730 / 0.1440D4 + t435 * t731 / 0.1440D4 + t18 * t75
     #1 * t65 / 0.16D2 - (0.90D2 * t17 * 0.3141592653589793D1 * (-t730 +
     # t444 * t731) + t739) * t65 / 0.1440D4 + t18 * t731 * t65 * t24 / 
     #0.8D1 + (0.90D2 * t17 * 0.3141592653589793D1 * (t730 - t459 * t731
     #) - t739) * t24 / 0.720D3 + t18 * t751 * t24 / 0.8D1
      t779 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t9, 0.0D0, 0.0D0, t778)
      t781 = -t333 * t14 * t179 * t160 * t328 * t183 / 0.16D2 + t362 * t
     #361 + t385 * t384 + t472 * t471 + t496 * t495 + t520 * t519 + t544
     # * t543 + t595 * t594 + t618 * t617 + t641 * t640 + t665 * t664 + 
     #t716 * t715 - t723 * t14 * t179 * t160 * t718 * t183 / 0.16D2 + t7
     #79 * t778
      rrqg2qght8s2em1 = t327 + t781

      end function



      doubleprecision function rrqg2qght8s2em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.10D
     #1, x4)
      t8 = 0.3141592653589793D1 * t7
      t9 = 0.1D1 / x3
      t13 = 0.1D1 / x2
      t17 = 0.1D1 / x1
      t21 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.10
     #D1, x4)
      t27 = z ** 2
      t30 = Sin(x4 * 0.3141592653589793D1)
      t31 = t30 ** 2
      t33 = t1 ** 2
      t34 = t33 ** 2
      t37 = log(0.4D1 / t27 * t31 * t34)
      t42 = (-0.180D3 * t6 * lh - 0.90D2 * t37 * t3 * t5) * 0.3141592653
     #589793D1
      t45 = t6 * t8 * t9 / 0.16D2 + t6 * t8 * t13 / 0.16D2 + t6 * t8 * t
     #17 / 0.8D1 + t6 * 0.3141592653589793D1 * t21 / 0.16D2 + t42 * t7 /
     # 0.1440D4
      t46 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t45)
      t48 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.10
     #D1, x4)
      t49 = 0.3141592653589793D1 * t48
      t59 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.10
     #D1, x4)
      t65 = t6 * t49 * t9 / 0.16D2 + t6 * t49 * t13 / 0.16D2 + t6 * t49 
     #* t17 / 0.8D1 + t6 * 0.3141592653589793D1 * t59 / 0.16D2 + t42 * t
     #48 / 0.1440D4
      t66 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t65)
      t68 = t2 * x1
      t69 = -0.1D1 + x1
      t70 = t2 * t69
      t71 = -t69
      t72 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, t71, 0.0D0, 0.10D1,
     # x4)
      t74 = 0.3141592653589793D1 * t72 * t17
      t77 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t68, -t70, 0.0D0, -t6 * t74 
     #/ 0.8D1)
      t82 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, t71, 0.0D0, 0.10D1,
     # x4)
      t84 = 0.3141592653589793D1 * t82 * t17
      t87 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t70, t68, 0.0D0, -t6 * t84 
     #/ 0.8D1)
      t92 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.10
     #D1, x4)
      t93 = 0.3141592653589793D1 * t92
      t103 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t109 = t6 * t93 * t9 / 0.16D2 + t6 * t93 * t13 / 0.16D2 + t6 * t93
     # * t17 / 0.8D1 + t6 * 0.3141592653589793D1 * t103 / 0.16D2 + t42 *
     # t92 / 0.1440D4
      t110 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t109)
      t112 = t2 * x3
      t113 = -0.1D1 + x3
      t114 = t2 * t113
      t115 = -t113
      t116 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t11
     #5, x4)
      t118 = 0.3141592653589793D1 * t116 * t9
      t121 = FJET(XB1, XB2, s, 0.0D0, t112, 0.0D0, -t114, 0.0D0, -t6 * t
     #118 / 0.16D2)
      t127 = x2 * s * t1
      t130 = (-0.1D1 + x2) * s * t1
      t131 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t133 = 0.3141592653589793D1 * t131 * t13
      t136 = FJET(XB1, XB2, s, 0.0D0, t127, 0.0D0, -t130, 0.0D0, -t6 * t
     #133 / 0.16D2)
      t141 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t11
     #5, x4)
      t143 = 0.3141592653589793D1 * t141 * t9
      t146 = FJET(XB1, XB2, s, 0.0D0, -t114, 0.0D0, t112, 0.0D0, -t6 * t
     #143 / 0.16D2)
      t151 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t153 = 0.3141592653589793D1 * t151 * t13
      t156 = FJET(XB1, XB2, s, 0.0D0, -t130, 0.0D0, t127, 0.0D0, -t6 * t
     #153 / 0.16D2)
      t161 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t162 = 0.3141592653589793D1 * t161
      t171 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.1
     #0D1, x4)
      t178 = t6 * t162 * t9 / 0.16D2 + t42 * t161 / 0.1440D4 + t6 * t162
     # * t13 / 0.16D2 + t6 * 0.3141592653589793D1 * t171 / 0.16D2 + t6 *
     # t162 * t17 / 0.8D1
      t179 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t178)
      t181 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, t71, 0.0D0, 0.10D1
     #, x4)
      t183 = 0.3141592653589793D1 * t181 * t17
      t186 = FJET(XB1, XB2, s, t68, -t70, 0.0D0, 0.0D0, 0.0D0, -t6 * t18
     #3 / 0.8D1)
      t191 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t11
     #5, x4)
      t193 = 0.3141592653589793D1 * t191 * t9
      t196 = FJET(XB1, XB2, s, t112, 0.0D0, -t114, 0.0D0, 0.0D0, -t6 * t
     #193 / 0.16D2)
      t201 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t203 = 0.3141592653589793D1 * t201 * t13
      t206 = FJET(XB1, XB2, s, t127, 0.0D0, -t130, 0.0D0, 0.0D0, -t6 * t
     #203 / 0.16D2)
      t211 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, t11
     #5, x4)
      t213 = 0.3141592653589793D1 * t211 * t9
      t216 = FJET(XB1, XB2, s, -t114, 0.0D0, t112, 0.0D0, 0.0D0, -t6 * t
     #213 / 0.16D2)
      t221 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, t71, 0.0D0, 0.10D1
     #, x4)
      t223 = 0.3141592653589793D1 * t221 * t17
      t226 = FJET(XB1, XB2, s, -t70, t68, 0.0D0, 0.0D0, 0.0D0, -t6 * t22
     #3 / 0.8D1)
      t231 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.10D1
     #, x4)
      t233 = 0.3141592653589793D1 * t231 * t13
      t236 = FJET(XB1, XB2, s, -t130, 0.0D0, t127, 0.0D0, 0.0D0, -t6 * t
     #233 / 0.16D2)
      rrqg2qght8s2em2 = t46 * t45 + t66 * t65 - t77 * t3 * t5 * t74 / 0.
     #8D1 - t87 * t3 * t5 * t84 / 0.8D1 + t110 * t109 - t121 * t3 * t5 *
     # t118 / 0.16D2 - t136 * t3 * t5 * t133 / 0.16D2 - t146 * t3 * t5 *
     # t143 / 0.16D2 - t156 * t3 * t5 * t153 / 0.16D2 + t179 * t178 - t1
     #86 * t3 * t5 * t183 / 0.8D1 - t196 * t3 * t5 * t193 / 0.16D2 - t20
     #6 * t3 * t5 * t203 / 0.16D2 - t216 * t3 * t5 * t213 / 0.16D2 - t22
     #6 * t3 * t5 * t223 / 0.8D1 - t236 * t3 * t5 * t233 / 0.16D2

      end function



      doubleprecision function rrqg2qght8s2em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.10D
     #1, x4)
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t6 * 0.314
     #1592653589793D1 * t7 / 0.16D2)
      t13 = t5 * 0.3141592653589793D1
      t16 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.10
     #D1, x4)
      t20 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t6 * 0.314
     #1592653589793D1 * t16 / 0.16D2)
      t24 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.10
     #D1, x4)
      t28 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t6 * 0.314
     #1592653589793D1 * t24 / 0.16D2)
      t32 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.10
     #D1, x4)
      t36 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t6 * 0.314
     #1592653589793D1 * t32 / 0.16D2)
      rrqg2qght8s2em3 = t11 * t3 * t13 * t7 / 0.16D2 + t20 * t3 * t13 * 
     #t16 / 0.16D2 + t28 * t3 * t13 * t24 / 0.16D2 + t36 * t3 * t13 * t3
     #2 / 0.16D2

      end function



      doubleprecision function rrqg2qght8s2em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6
      rrqg2qght8s2em4 = 0.0D0

      end function


      doubleprecision function rrqg2qght8s3e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = rrqg2qgh82J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10D
     #1, x4)
      t11 = z ** 2
      t13 = 0.1D1 / t11 / z
      t14 = x4 * 0.3141592653589793D1
      t15 = Sin(t14)
      t16 = t15 ** 2
      t17 = t13 * t16
      t18 = t1 ** 2
      t19 = t18 ** 2
      t20 = t17 * t19
      t22 = log(0.4D1 * t20)
      t23 = t22 ** 2
      t24 = t23 * t3
      t25 = t5 * lh
      t28 = 0.3141592653589793D1 ** 2
      t31 = lh ** 2
      t34 = 0.60D2 * lh * t28 - 0.2884936567583026D3 - 0.120D3 * t31 * l
     #h
      t37 = t23 * t22 * t3
      t40 = t22 * t3
      t43 = 0.180D3 * t31 - 0.30D2 * t28
      t44 = t5 * t43
      t47 = (-0.90D2 * t24 * t25 + t6 * t34 - 0.15D2 * t37 * t5 - t40 * 
     #t44) * 0.3141592653589793D1
      t48 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t55 = t6 * t43
      t57 = (0.180D3 * t40 * t25 + 0.45D2 * t24 * t5 + t55) * 0.31415926
     #53589793D1
      t58 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t61 = t6 * lh
      t66 = (-0.180D3 * t61 - 0.90D2 * t40 * t5) * 0.3141592653589793D1
      t67 = rrqg2qgh82J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t76 = t28 ** 2
      t77 = t31 ** 2
      t84 = t23 ** 2
      t89 = (0.30D2 * t37 * t25 + t24 * t44 / 0.2D1 - t40 * t5 * t34 + t
     #6 * (t76 + 0.60D2 * t77 + 0.5769873135166051D3 * lh - 0.60D2 * t31
     # * t28) + 0.15D2 / 0.4D1 * t84 * t3 * t5) * 0.3141592653589793D1
      t90 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t93 = x1 ** 2
      t94 = x3 * t93
      t97 = log(0.4D1 * t94 * t20)
      t99 = z * t58
      t100 = t94 * t16
      t101 = t13 * t19
      t102 = -0.1D1 + x3
      t103 = 0.1D1 / t102
      t107 = log(-0.4D1 * t100 * t101 * t103)
      t108 = t107 * z
      t110 = t107 ** 2
      t111 = t110 * z
      t115 = cos(t14)
      t116 = x3 * z
      t118 = Sqrt(-t116 * t102)
      t122 = 0.1D1 / (-z - x3 + 0.2D1 * t115 * t118)
      t124 = t97 ** 2
      t131 = 0.3141592653589793D1 * lh
      t132 = z * t48
      t141 = t43 * 0.3141592653589793D1
      t143 = z * t90 * t122
      t148 = 0.1D1 / x3
      t150 = 0.1D1 / x1
      t153 = t93 * t16
      t156 = log(0.4D1 * t153 * t101)
      t161 = t156 ** 2
      t164 = t161 * t156
      t172 = t34 * 0.3141592653589793D1
      t174 = t6 * t172 * t90
      t185 = x2 ** 2
      t186 = x3 * t185
      t187 = t186 * t93
      t192 = log(-0.4D1 * t187 * t17 * t19 * t103)
      t193 = t192 * z
      t197 = 0.1D1 - x2
      t198 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t197, 0.10D
     #1, x4)
      t199 = -t197
      t200 = t19 * t199
      t204 = log(-0.4D1 * t187 * t17 * t200)
      t205 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t197, 0.10D
     #1, x4)
      t209 = log(0.4D1 * t187 * t20)
      t215 = -t143 - t90 + t205
      t221 = 0.1D1 / x2
      t222 = t221 * t150
      t225 = t185 * t93
      t228 = log(0.4D1 * t225 * t20)
      t230 = t228 ** 2
      t233 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t197, 0.10D
     #1, x4)
      t234 = t225 * t16
      t238 = log(-0.4D1 * t234 * t101 * t199)
      t240 = t238 ** 2
      t253 = -t205 + t90
      t269 = x3 * t13
      t270 = t16 * t19
      t271 = t270 * t103
      t274 = log(-0.4D1 * t269 * t271)
      t279 = log(0.4D1 * t269 * t270)
      t280 = t274 * z * t122 + t279
      t283 = t279 ** 2
      t285 = t274 ** 2
      t290 = t283 * t279 / 0.6D1 + t285 * t274 * z * t122 / 0.6D1
      t304 = -z * t122 - 0.1D1
      t316 = -t285 * z * t122 / 0.2D1 - t283 / 0.2D1
      t323 = log(0.4D1 * t186 * t20)
      t325 = t323 ** 2
      t328 = t186 * t13
      t329 = t270 * t199
      t332 = log(-0.4D1 * t328 * t329)
      t334 = t332 ** 2
      t339 = log(-0.4D1 * t328 * t271)
      t340 = t339 * z
      t342 = t339 ** 2
      t343 = t342 * z
      t367 = t13 * t185
      t370 = log(0.4D1 * t367 * t270)
      t374 = log(-0.4D1 * t367 * t329)
      t379 = t374 ** 2
      t382 = t379 * t374
      t386 = t370 ** 2
      t389 = t386 * t370
      t392 = rrqg2qgh82J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t197, 0.10D
     #1, x4)
      t413 = t6 * 0.3141592653589793D1 * t7 / 0.32D2 + t47 * t48 / 0.288
     #0D4 + t57 * t58 / 0.2880D4 + t66 * t67 / 0.2880D4 + t89 * t90 / 0.
     #2880D4 - (0.90D2 * t6 * 0.3141592653589793D1 * (-t58 + t97 * t48 -
     # (t99 - t108 * t48 + t111 * t90 / 0.2D1) * t122 - t124 * t90 / 0.2
     #D1) - 0.180D3 * t6 * t131 * (-t48 - (t132 - t108 * t90) * t122 + t
     #97 * t90) + t6 * t141 * (-t90 - t143)) * t148 * t150 / 0.1440D4 + 
     #(t6 * t141 * (t48 - t156 * t90) + 0.90D2 * t6 * 0.3141592653589793
     #D1 * (t161 * t48 / 0.2D1 - t164 * t90 / 0.6D1 + t67 - t156 * t58) 
     #+ t174 - 0.180D3 * t6 * t131 * (t58 + t161 * t90 / 0.2D1 - t156 * 
     #t48)) * t150 / 0.1440D4 - (0.90D2 * t6 * 0.3141592653589793D1 * (-
     #(t132 - t193 * t90) * t122 + t198 - t204 * t205 + t209 * t90 - t48
     #) - 0.180D3 * t6 * t131 * t215) * t148 * t222 / 0.720D3 + (0.90D2 
     #* t6 * 0.3141592653589793D1 * (t58 - t228 * t48 + t230 * t90 / 0.2
     #D1 - t233 + t238 * t198 - t240 * t205 / 0.2D1) - 0.180D3 * t6 * t1
     #31 * (t48 + t238 * t205 - t228 * t90 - t198) + t6 * t141 * t253) *
     # t221 * t150 / 0.720D3 - ((0.90D2 * t6 * 0.3141592653589793D1 * t5
     #8 - 0.180D3 * t6 * t131 * t48 + t6 * t141 * t90) * t280 + 0.90D2 *
     # t6 * 0.3141592653589793D1 * t90 * t290 + (t6 * t141 * t48 + 0.90D
     #2 * t6 * 0.3141592653589793D1 * t67 + t174 - 0.180D3 * t6 * t131 *
     # t58) * t304 + (0.90D2 * t6 * 0.3141592653589793D1 * t48 - 0.180D3
     # * t6 * t131 * t90) * t316) * t148 / 0.2880D4 - (0.90D2 * t6 * 0.3
     #141592653589793D1 * (t323 * t48 - t325 * t90 / 0.2D1 + t233 - t332
     # * t198 - t58 + t334 * t205 / 0.2D1 - (t99 - t340 * t48 + t343 * t
     #90 / 0.2D1) * t122) - 0.180D3 * t6 * t131 * (-t48 + t323 * t90 + t
     #198 - t332 * t205 - (t132 - t340 * t90) * t122) + t6 * t141 * t215
     #) * t148 * t221 / 0.1440D4 + (t6 * t141 * (t48 - t370 * t90 - t198
     # + t374 * t205) + 0.90D2 * t6 * 0.3141592653589793D1 * (-t379 * t1
     #98 / 0.2D1 + t382 * t205 / 0.6D1 + t374 * t233 + t67 + t386 * t48 
     #/ 0.2D1 - t389 * t90 / 0.6D1 - t392 - t370 * t58) + t6 * t172 * t2
     #53 - 0.180D3 * t6 * t131 * (-t370 * t48 + t386 * t90 / 0.2D1 - t23
     #3 + t374 * t198 + t58 - t379 * t205 / 0.2D1)) * t221 / 0.1440D4
      t414 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t413)
      t416 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t418 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t197, 0.10D
     #1, x4)
      t420 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t421 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t197, 0.10D
     #1, x4)
      t422 = z * t420
      t424 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t431 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t197, 0.10D
     #1, x4)
      t439 = z * t416
      t449 = z * t424 * t122
      t450 = -t449 + t431 - t424
      t462 = rrqg2qgh84J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t197, 0.10D
     #1, x4)
      t470 = rrqg2qgh84J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t478 = -t431 + t424
      t494 = rrqg2qgh84J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t526 = t6 * t172 * t424
      t631 = -(0.90D2 * t6 * 0.3141592653589793D1 * (t323 * t416 - t332 
     #* t418 - t420 + t421 - (t422 - t340 * t416 + t343 * t424 / 0.2D1) 
     #* t122 - t325 * t424 / 0.2D1 + t334 * t431 / 0.2D1) - 0.180D3 * t6
     # * t131 * (t323 * t424 - (t439 - t340 * t424) * t122 - t332 * t431
     # - t416 + t418) + t6 * t141 * t450) * t148 * t221 / 0.1440D4 + (t6
     # * t141 * (-t370 * t424 + t374 * t431 + t416 - t418) + 0.90D2 * t6
     # * 0.3141592653589793D1 * (-t462 - t379 * t418 / 0.2D1 + t374 * t4
     #21 + t382 * t431 / 0.6D1 + t386 * t416 / 0.2D1 + t470 - t370 * t42
     #0 - t389 * t424 / 0.6D1) + t6 * t172 * t478 - 0.180D3 * t6 * t131 
     #* (t386 * t424 / 0.2D1 - t421 - t370 * t416 + t374 * t418 + t420 -
     # t379 * t431 / 0.2D1)) * t221 / 0.1440D4 + t6 * 0.3141592653589793
     #D1 * t494 / 0.32D2 + t47 * t416 / 0.2880D4 + t57 * t420 / 0.2880D4
     # + t66 * t470 / 0.2880D4 + t89 * t424 / 0.2880D4 - ((0.90D2 * t6 *
     # 0.3141592653589793D1 * t420 - 0.180D3 * t6 * t131 * t416 + t6 * t
     #141 * t424) * t280 + 0.90D2 * t6 * 0.3141592653589793D1 * t424 * t
     #290 + (t6 * t141 * t416 + 0.90D2 * t6 * 0.3141592653589793D1 * t47
     #0 + t526 - 0.180D3 * t6 * t131 * t420) * t304 + (0.90D2 * t6 * 0.3
     #141592653589793D1 * t416 - 0.180D3 * t6 * t131 * t424) * t316) * t
     #148 / 0.2880D4 - (0.90D2 * t6 * 0.3141592653589793D1 * (-t124 * t4
     #24 / 0.2D1 - t420 - (t422 - t108 * t416 + t111 * t424 / 0.2D1) * t
     #122 + t97 * t416) - 0.180D3 * t6 * t131 * (-t416 - (t439 - t108 * 
     #t424) * t122 + t97 * t424) + t6 * t141 * (-t424 - t449)) * t148 * 
     #t150 / 0.1440D4 + (t6 * t141 * (t416 - t156 * t424) + 0.90D2 * t6 
     #* 0.3141592653589793D1 * (t161 * t416 / 0.2D1 - t156 * t420 + t470
     # - t164 * t424 / 0.6D1) + t526 - 0.180D3 * t6 * t131 * (-t156 * t4
     #16 + t161 * t424 / 0.2D1 + t420)) * t150 / 0.1440D4 - (0.90D2 * t6
     # * 0.3141592653589793D1 * (-t416 - (t439 - t193 * t424) * t122 - t
     #204 * t431 + t209 * t424 + t418) - 0.180D3 * t6 * t131 * t450) * t
     #148 * t222 / 0.720D3 + (0.90D2 * t6 * 0.3141592653589793D1 * (t230
     # * t424 / 0.2D1 - t240 * t431 / 0.2D1 - t421 - t228 * t416 + t238 
     #* t418 + t420) - 0.180D3 * t6 * t131 * (-t418 + t238 * t431 + t416
     # - t228 * t424) + t6 * t141 * t478) * t221 * t150 / 0.720D3
      t632 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t631)
      t634 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t637 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t638 = z * t637
      t639 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t650 = z * t639
      t660 = z * t634 * t122
      t672 = rrqg2qgh81J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t683 = t6 * t172 * t634
      t694 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t197, 0.10D
     #1, x4)
      t700 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t197, 0.10D
     #1, x4)
      t705 = -t634 - t660 + t694
      t717 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t197, 0.10D
     #1, x4)
      t730 = t634 - t694
      t777 = rrqg2qgh81J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t197, 0.10D
     #1, x4)
      t802 = rrqg2qgh81J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t849 = -(0.90D2 * t6 * 0.3141592653589793D1 * (-t124 * t634 / 0.2D
     #1 - (t638 - t108 * t639 + t111 * t634 / 0.2D1) * t122 + t97 * t639
     # - t637) - 0.180D3 * t6 * t131 * (-t639 - (t650 - t108 * t634) * t
     #122 + t97 * t634) + t6 * t141 * (-t634 - t660)) * t148 * t150 / 0.
     #1440D4 + (t6 * t141 * (-t156 * t634 + t639) + 0.90D2 * t6 * 0.3141
     #592653589793D1 * (t672 + t161 * t639 / 0.2D1 - t164 * t634 / 0.6D1
     # - t156 * t637) + t683 - 0.180D3 * t6 * t131 * (t161 * t634 / 0.2D
     #1 + t637 - t156 * t639)) * t150 / 0.1440D4 - (0.90D2 * t6 * 0.3141
     #592653589793D1 * (-t204 * t694 - (t650 - t193 * t634) * t122 - t63
     #9 + t209 * t634 + t700) - 0.180D3 * t6 * t131 * t705) * t148 * t22
     #2 / 0.720D3 + (0.90D2 * t6 * 0.3141592653589793D1 * (-t240 * t694 
     #/ 0.2D1 + t238 * t700 - t228 * t639 + t637 - t717 + t230 * t634 / 
     #0.2D1) - 0.180D3 * t6 * t131 * (-t700 - t228 * t634 + t639 + t238 
     #* t694) + t6 * t141 * t730) * t221 * t150 / 0.720D3 - (0.90D2 * t6
     # * 0.3141592653589793D1 * (-(t638 - t340 * t639 + t343 * t634 / 0.
     #2D1) * t122 + t717 - t637 + t334 * t694 / 0.2D1 - t332 * t700 - t3
     #25 * t634 / 0.2D1 + t323 * t639) - 0.180D3 * t6 * t131 * (-(t650 -
     # t340 * t634) * t122 - t639 - t332 * t694 + t323 * t634 + t700) + 
     #t6 * t141 * t705) * t148 * t221 / 0.1440D4 + (t6 * t141 * (t639 - 
     #t370 * t634 - t700 + t374 * t694) + 0.90D2 * t6 * 0.31415926535897
     #93D1 * (-t389 * t634 / 0.6D1 - t370 * t637 + t386 * t639 / 0.2D1 -
     # t777 + t382 * t694 / 0.6D1 + t672 - t379 * t700 / 0.2D1 + t374 * 
     #t717) + t6 * t172 * t730 - 0.180D3 * t6 * t131 * (t386 * t634 / 0.
     #2D1 + t637 - t370 * t639 - t717 + t374 * t700 - t379 * t694 / 0.2D
     #1)) * t221 / 0.1440D4 + t6 * 0.3141592653589793D1 * t802 / 0.32D2 
     #+ t47 * t639 / 0.2880D4 + t57 * t637 / 0.2880D4 + t66 * t672 / 0.2
     #880D4 - ((0.90D2 * t6 * 0.3141592653589793D1 * t637 - 0.180D3 * t6
     # * t131 * t639 + t6 * t141 * t634) * t280 + 0.90D2 * t6 * 0.314159
     #2653589793D1 * t634 * t290 + (t6 * t141 * t639 + 0.90D2 * t6 * 0.3
     #141592653589793D1 * t672 + t683 - 0.180D3 * t6 * t131 * t637) * t3
     #04 + (0.90D2 * t6 * 0.3141592653589793D1 * t639 - 0.180D3 * t6 * t
     #131 * t634) * t316) * t148 / 0.2880D4 + t89 * t634 / 0.2880D4
      t850 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t849)
      t852 = x2 * x3
      t853 = 0.1D1 - x3 + t852
      t854 = 0.1D1 / t853
      t855 = t852 * t854
      t856 = t2 * t855
      t857 = t102 * t854
      t858 = t2 * t857
      t859 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t197, -t857
     #, x4)
      t860 = z * t859
      t861 = t186 * t153
      t862 = t199 * t102
      t863 = t853 ** 2
      t864 = 0.1D1 / t863
      t865 = t862 * t864
      t869 = log(0.4D1 * t861 * t101 * t865)
      t870 = t869 * z
      t871 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t197, -t857
     #, x4)
      t876 = Sqrt(t116 * t862)
      t880 = 0.1D1 / (-z - x3 + t852 + 0.2D1 * t115 * t876)
      t884 = 0.3141592653589793D1 * z
      t886 = t884 * t871 * t880
      t893 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t197, -t857
     #, x4)
      t900 = log(0.4D1 * t186 * t17 * t200 * t102 * t864)
      t901 = t900 * z
      t903 = t900 ** 2
      t904 = t903 * z
      t923 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (-t860 + t870 * t87
     #1) * t880 - 0.180D3 * t61 * t886) * t148 * t222 / 0.720D3 - (-0.90
     #D2 * t6 * 0.3141592653589793D1 * (-z * t893 + t901 * t859 - t904 *
     # t871 / 0.2D1) * t880 + 0.180D3 * t61 * 0.3141592653589793D1 * (-t
     #860 + t901 * t871) * t880 + t55 * t886) * t148 * t221 / 0.1440D4
      t924 = FJET(XB1, XB2, s, 0.0D0, t856, 0.0D0, -t858, 0.0D0, t923)
      t927 = t1 * x1
      t928 = x1 * z
      t929 = -z - x1 + t928
      t930 = 0.1D1 / t929
      t932 = t199 * s * t927 * t930
      t933 = -0.1D1 + x1
      t934 = t2 * t933
      t936 = x2 * s * t927
      t937 = s * t18
      t940 = x1 * t933 * t930
      t941 = t937 * t199 * t940
      t942 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, x1, t197, 0.10D1, 
     #x4)
      t943 = 0.1D1 / t11
      t944 = t943 * t19
      t945 = t933 ** 2
      t946 = t930 * t945
      t951 = log(0.4D1 * t861 * t944 * t946 * t199)
      t952 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, x1, t197, 0.10D1, 
     #x4)
      t964 = t16 * t943
      t966 = t19 * t930
      t971 = log(0.4D1 * t225 * t964 * t966 * t945 * t199)
      t973 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, x1, t197, 0.10D1, 
     #x4)
      t974 = t971 ** 2
      t992 = -(0.90D2 * t6 * 0.3141592653589793D1 * (-t942 + t951 * t952
     #) + 0.180D3 * t6 * t131 * t952) * t148 * t222 / 0.720D3 + (0.90D2 
     #* t6 * 0.3141592653589793D1 * (-t971 * t942 + t973 + t974 * t952 /
     # 0.2D1) - 0.180D3 * t6 * t131 * (-t971 * t952 + t942) + t6 * t141 
     #* t952) * t221 * t150 / 0.720D3
      t993 = FJET(XB1, XB2, s, 0.0D0, t932, -t934, t936, -t941, t992)
      t996 = t2 * x1 * t930
      t997 = t937 * t940
      t998 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t999 = t944 * t946
      t1002 = log(-0.4D1 * t100 * t999)
      t1003 = t1002 ** 2
      t1004 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1007 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1009 = z * t929
      t1016 = log(0.4D1 * t94 * t964 * t966 * t945 * t103)
      t1017 = t1016 * z
      t1020 = t1016 ** 2
      t1021 = t1020 * z
      t1022 = t929 * t1004
      t1026 = x3 * x1
      t1027 = t1026 * z
      t1029 = 0.2D1 * t94 * z
      t1030 = x1 * t11
      t1031 = x3 * t11
      t1032 = t1031 * x1
      t1033 = t94 * t11
      t1034 = x3 * t929
      t1036 = Sqrt(t1034 * t102)
      t1041 = 0.1D1 / (-t928 - t1027 - t116 + t1029 - t94 + t1030 + t103
     #2 - t1033 - t11 + 0.2D1 * t115 * t1036 * z)
      t1047 = t1009 * t1007
      t1058 = -t1009 * t1004 * t1041 + t1004
      t1065 = t153 * t943
      t1066 = t966 * t945
      t1069 = log(-0.4D1 * t1065 * t1066)
      t1074 = t1069 ** 2
      t1077 = t1074 * t1069
      t1080 = rrqg2qgh81J4(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1102 = log(0.4D1 * t861 * t944 * t946 * t103)
      t1103 = t1102 * z
      t1109 = log(-0.4D1 * t861 * t999)
      t1124 = log(-0.4D1 * t234 * t999)
      t1125 = t1124 ** 2
      t1144 = -(0.90D2 * t6 * 0.3141592653589793D1 * (t998 + t1003 * t10
     #04 / 0.2D1 - t1002 * t1007 - (t1009 * t998 - t1017 * t929 * t1007 
     #+ t1021 * t1022 / 0.2D1) * t1041) - 0.180D3 * t6 * t131 * (-(t1047
     # - t1017 * t1022) * t1041 + t1007 - t1002 * t1004) + t6 * t141 * t
     #1058) * t148 * t150 / 0.1440D4 + (t6 * t141 * (-t1007 + t1069 * t1
     #004) + 0.90D2 * t6 * 0.3141592653589793D1 * (-t1074 * t1007 / 0.2D
     #1 + t1077 * t1004 / 0.6D1 - t1080 + t1069 * t998) - t6 * t172 * t1
     #004 - 0.180D3 * t6 * t131 * (-t998 - t1074 * t1004 / 0.2D1 + t1069
     # * t1007)) * t150 / 0.1440D4 - (0.90D2 * t6 * 0.3141592653589793D1
     # * (-(t1047 - t1103 * t1022) * t1041 - t1109 * t1004 + t1007) - 0.
     #180D3 * t6 * t131 * t1058) * t148 * t222 / 0.720D3 + (0.90D2 * t6 
     #* 0.3141592653589793D1 * (-t998 - t1125 * t1004 / 0.2D1 + t1124 * 
     #t1007) - 0.180D3 * t6 * t131 * (-t1007 + t1124 * t1004) - t6 * t14
     #1 * t1004) * t221 * t150 / 0.720D3
      t1145 = FJET(XB1, XB2, s, 0.0D0, -t934, -t996, 0.0D0, t997, t1144)
      t1147 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1149 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1152 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1153 = t929 * t1152
      t1166 = t1009 * t1149
      t1176 = t1152 - t1009 * t1152 * t1041
      t1189 = rrqg2qgh84J4(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1242 = -(0.90D2 * t6 * 0.3141592653589793D1 * (t1147 - (t1009 * t
     #1147 - t1017 * t929 * t1149 + t1021 * t1153 / 0.2D1) * t1041 + t10
     #03 * t1152 / 0.2D1 - t1002 * t1149) - 0.180D3 * t6 * t131 * (t1149
     # - t1002 * t1152 - (t1166 - t1017 * t1153) * t1041) + t6 * t141 * 
     #t1176) * t148 * t150 / 0.1440D4 + (t6 * t141 * (-t1149 + t1069 * t
     #1152) + 0.90D2 * t6 * 0.3141592653589793D1 * (-t1074 * t1149 / 0.2
     #D1 - t1189 + t1077 * t1152 / 0.6D1 + t1069 * t1147) - t6 * t172 * 
     #t1152 - 0.180D3 * t6 * t131 * (-t1074 * t1152 / 0.2D1 - t1147 + t1
     #069 * t1149)) * t150 / 0.1440D4 - (0.90D2 * t6 * 0.314159265358979
     #3D1 * (t1149 - (t1166 - t1103 * t1153) * t1041 - t1109 * t1152) - 
     #0.180D3 * t6 * t131 * t1176) * t148 * t222 / 0.720D3 + (0.90D2 * t
     #6 * 0.3141592653589793D1 * (-t1125 * t1152 / 0.2D1 + t1124 * t1149
     # - t1147) - 0.180D3 * t6 * t131 * (t1124 * t1152 - t1149) - t6 * t
     #141 * t1152) * t221 * t150 / 0.720D3
      t1243 = FJET(XB1, XB2, s, 0.0D0, -t996, -t934, 0.0D0, t997, t1242)
      t1245 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t197, -t85
     #7, x4)
      t1246 = z * t1245
      t1247 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t197, -t85
     #7, x4)
      t1255 = t884 * t1247 * t880
      t1262 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t197, -t85
     #7, x4)
      t1283 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (-t1246 + t870 * t
     #1247) * t880 - 0.180D3 * t61 * t1255) * t148 * t222 / 0.720D3 - (-
     #0.90D2 * t6 * 0.3141592653589793D1 * (-z * t1262 + t901 * t1245 - 
     #t904 * t1247 / 0.2D1) * t880 + 0.180D3 * t61 * 0.3141592653589793D
     #1 * (-t1246 + t901 * t1247) * t880 + t55 * t1255) * t148 * t221 / 
     #0.1440D4
      t1284 = FJET(XB1, XB2, s, 0.0D0, -t858, 0.0D0, t856, 0.0D0, t1283)
      t1286 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.
     #10D1, x4)
      t1290 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.
     #10D1, x4)
      t1294 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.
     #10D1, x4)
      t1305 = rrqg2qgh83J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.
     #10D1, x4)
      t1310 = t6 * t172 * t1294
      t1329 = z * t1286
      t1340 = z * t1290
      t1350 = z * t1294 * t122
      t1381 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t197, 0.10
     #D1, x4)
      t1384 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t197, 0.10
     #D1, x4)
      t1392 = -t1294 + t1381 - t1350
      t1404 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t197, 0.10
     #D1, x4)
      t1417 = -t1381 + t1294
      t1424 = rrqg2qgh83J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.
     #10D1, x4)
      t1468 = rrqg2qgh83J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t197, 0.10
     #D1, x4)
      t1501 = -((0.90D2 * t6 * 0.3141592653589793D1 * t1286 - 0.180D3 * 
     #t6 * t131 * t1290 + t6 * t141 * t1294) * t280 + 0.90D2 * t6 * 0.31
     #41592653589793D1 * t1294 * t290 + (t6 * t141 * t1290 + 0.90D2 * t6
     # * 0.3141592653589793D1 * t1305 + t1310 - 0.180D3 * t6 * t131 * t1
     #286) * t304 + (0.90D2 * t6 * 0.3141592653589793D1 * t1290 - 0.180D
     #3 * t6 * t131 * t1294) * t316) * t148 / 0.2880D4 - (0.90D2 * t6 * 
     #0.3141592653589793D1 * (-t1286 - t124 * t1294 / 0.2D1 - (t1329 - t
     #108 * t1290 + t111 * t1294 / 0.2D1) * t122 + t97 * t1290) - 0.180D
     #3 * t6 * t131 * (-(t1340 - t108 * t1294) * t122 - t1290 + t97 * t1
     #294) + t6 * t141 * (-t1350 - t1294)) * t148 * t150 / 0.1440D4 + (t
     #6 * t141 * (t1290 - t156 * t1294) + 0.90D2 * t6 * 0.31415926535897
     #93D1 * (t1305 - t164 * t1294 / 0.6D1 - t156 * t1286 + t161 * t1290
     # / 0.2D1) + t1310 - 0.180D3 * t6 * t131 * (t1286 - t156 * t1290 + 
     #t161 * t1294 / 0.2D1)) * t150 / 0.1440D4 - (0.90D2 * t6 * 0.314159
     #2653589793D1 * (-t1290 - t204 * t1381 + t209 * t1294 + t1384 - (t1
     #340 - t193 * t1294) * t122) - 0.180D3 * t6 * t131 * t1392) * t148 
     #* t222 / 0.720D3 + (0.90D2 * t6 * 0.3141592653589793D1 * (t230 * t
     #1294 / 0.2D1 - t240 * t1381 / 0.2D1 - t1404 - t228 * t1290 + t238 
     #* t1384 + t1286) - 0.180D3 * t6 * t131 * (t1290 + t238 * t1381 - t
     #228 * t1294 - t1384) + t6 * t141 * t1417) * t221 * t150 / 0.720D3 
     #+ t6 * 0.3141592653589793D1 * t1424 / 0.32D2 + t89 * t1294 / 0.288
     #0D4 - (0.90D2 * t6 * 0.3141592653589793D1 * (-(t1329 - t340 * t129
     #0 + t343 * t1294 / 0.2D1) * t122 + t323 * t1290 + t334 * t1381 / 0
     #.2D1 - t1286 - t332 * t1384 - t325 * t1294 / 0.2D1 + t1404) - 0.18
     #0D3 * t6 * t131 * (-(t1340 - t340 * t1294) * t122 - t1290 + t1384 
     #+ t323 * t1294 - t332 * t1381) + t6 * t141 * t1392) * t148 * t221 
     #/ 0.1440D4 + (t6 * t141 * (-t370 * t1294 - t1384 + t374 * t1381 + 
     #t1290) + 0.90D2 * t6 * 0.3141592653589793D1 * (-t379 * t1384 / 0.2
     #D1 + t374 * t1404 - t1468 - t370 * t1286 + t1305 - t389 * t1294 / 
     #0.6D1 + t382 * t1381 / 0.6D1 + t386 * t1290 / 0.2D1) + t6 * t172 *
     # t1417 - 0.180D3 * t6 * t131 * (t374 * t1384 + t1286 + t386 * t129
     #4 / 0.2D1 - t1404 - t370 * t1290 - t379 * t1381 / 0.2D1)) * t221 /
     # 0.1440D4 + t57 * t1286 / 0.2880D4 + t66 * t1305 / 0.2880D4 + t47 
     #* t1290 / 0.2880D4
      t1502 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t1501)
      t1504 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, x1, t197, 0.10D1,
     # x4)
      t1506 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, x1, t197, 0.10D1,
     # x4)
      t1517 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, x1, t197, 0.10D1,
     # x4)
      t1536 = -(0.90D2 * t6 * 0.3141592653589793D1 * (t951 * t1504 - t15
     #06) + 0.180D3 * t6 * t131 * t1504) * t148 * t222 / 0.720D3 + (0.90
     #D2 * t6 * 0.3141592653589793D1 * (t1517 - t971 * t1506 + t974 * t1
     #504 / 0.2D1) - 0.180D3 * t6 * t131 * (t1506 - t971 * t1504) + t6 *
     # t141 * t1504) * t221 * t150 / 0.720D3
      t1537 = FJET(XB1, XB2, s, t936, -t934, t932, 0.0D0, -t941, t1536)
      t1539 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t197, -t85
     #7, x4)
      t1541 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t197, -t85
     #7, x4)
      t1543 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t197, -t85
     #7, x4)
      t1551 = z * t1541
      t1559 = t884 * t1543 * t880
      t1577 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (-z * t1539 + t901
     # * t1541 - t904 * t1543 / 0.2D1) * t880 + 0.180D3 * t61 * 0.314159
     #2653589793D1 * (-t1551 + t901 * t1543) * t880 + t55 * t1559) * t14
     #8 * t221 / 0.1440D4 - (-0.90D2 * t6 * 0.3141592653589793D1 * (-t15
     #51 + t870 * t1543) * t880 - 0.180D3 * t61 * t1559) * t148 * t222 /
     # 0.720D3
      t1578 = FJET(XB1, XB2, s, t856, 0.0D0, -t858, 0.0D0, 0.0D0, t1577)
      t1580 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, x1, t197, 0.10D1,
     # x4)
      t1581 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, x1, t197, 0.10D1,
     # x4)
      t1593 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, x1, t197, 0.10D1,
     # x4)
      t1612 = -(0.90D2 * t6 * 0.3141592653589793D1 * (-t1580 + t951 * t1
     #581) + 0.180D3 * t6 * t131 * t1581) * t148 * t222 / 0.720D3 + (0.9
     #0D2 * t6 * 0.3141592653589793D1 * (t1593 - t971 * t1580 + t974 * t
     #1581 / 0.2D1) - 0.180D3 * t6 * t131 * (t1580 - t971 * t1581) + t6 
     #* t141 * t1581) * t221 * t150 / 0.720D3
      t1613 = FJET(XB1, XB2, s, t932, 0.0D0, t936, -t934, -t941, t1612)
      t1618 = t102 * s * t1 * t933 * t854
      t1619 = t2 * x1
      t1621 = Sqrt(-t1034 * t862)
      t1622 = t115 * t1621
      t1628 = t1619 * x2 * (-x3 + t852 - z + t116 - x1 + t1026 + t928 - 
     #t1027 + 0.2D1 * t1622) * t930 * t854
      t1629 = t934 * t855
      t1632 = t186 * x1
      t1634 = t186 * t928
      t1638 = t1619 * (0.2D1 * t1622 * x2 + t1632 + t186 * z - x2 + t852
     # - t1634 + 0.1D1 - x3) * t930 * t854
      t1643 = log(-0.4D1 * t186 * t1065 * t1066 * t865)
      t1644 = t1643 * t929
      t1647 = x2 * x1
      t1658 = t1647 * z
      t1662 = t1634 - 0.2D1 * t852 * t928 + t1031 * t1647 + 0.2D1 * t94 
     #* x2 * z - t94 * t11 * x2 - 0.2D1 * t1622 * t1647 + t11 - t1632 - 
     #0.2D1 * t1622 * z + t1658 - t852 * z + 0.2D1 * t1622 * t1658 + t11
     #6
      t1663 = x2 * t93
      t1670 = t94 - t1030 + t928 + t1027 - t1029 - t1032 + t1033 + t1663
     # + t852 * x1 - t94 * x2 - t1647 * t11 - 0.2D1 * t1663 * z + t1663 
     #* t11
      t1672 = 0.1D1 / (t1662 + t1670)
      t1673 = -z + t1658 - t1647
      t1674 = t1672 * t1673
      t1675 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, x1, t197, -t857, 
     #x4)
      t1678 = t929 * t1672
      t1679 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, x1, t197, -t857, 
     #x4)
      t1686 = t6 * t131
      t1691 = 0.90D2 * t6 * 0.3141592653589793D1 * (-t1644 * t1674 * t16
     #75 + t1678 * t1673 * t1679) - 0.180D3 * t1686 * t1678 * t1673 * t1
     #675
      t1695 = FJET(XB1, XB2, s, t1618, t1628, -t1629, -t1638, -t941, -t1
     #691 * t148 * t222 / 0.720D3)
      t1698 = t148 * t221 * t150
      t1701 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, x1, t197, -t857, 
     #x4)
      t1704 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, x1, t197, -t857, 
     #x4)
      t1715 = 0.90D2 * t6 * 0.3141592653589793D1 * (-t1644 * t1674 * t17
     #01 + t1678 * t1673 * t1704) - 0.180D3 * t1686 * t1678 * t1673 * t1
     #701
      t1719 = FJET(XB1, XB2, s, t1628, t1618, -t1638, -t1629, -t941, -t1
     #715 * t148 * t222 / 0.720D3)
      t1723 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1726 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1728 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1731 = t929 * t1723
      t1742 = t1009 * t1728
      t1752 = t1723 - t1009 * t1723 * t1041
      t1767 = rrqg2qgh83J4(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1818 = -(0.90D2 * t6 * 0.3141592653589793D1 * (t1003 * t1723 / 0.
     #2D1 - (t1009 * t1726 - t1017 * t929 * t1728 + t1021 * t1731 / 0.2D
     #1) * t1041 - t1002 * t1728 + t1726) - 0.180D3 * t6 * t131 * (t1728
     # - t1002 * t1723 - (t1742 - t1017 * t1731) * t1041) + t6 * t141 * 
     #t1752) * t148 * t150 / 0.1440D4 + (t6 * t141 * (-t1728 + t1069 * t
     #1723) + 0.90D2 * t6 * 0.3141592653589793D1 * (-t1074 * t1728 / 0.2
     #D1 + t1077 * t1723 / 0.6D1 - t1767 + t1069 * t1726) - t6 * t172 * 
     #t1723 - 0.180D3 * t6 * t131 * (-t1074 * t1723 / 0.2D1 - t1726 + t1
     #069 * t1728)) * t150 / 0.1440D4 - (0.90D2 * t6 * 0.314159265358979
     #3D1 * (-t1109 * t1723 - (t1742 - t1103 * t1731) * t1041 + t1728) -
     # 0.180D3 * t6 * t131 * t1752) * t148 * t222 / 0.720D3 + (0.90D2 * 
     #t6 * 0.3141592653589793D1 * (-t1726 + t1124 * t1728 - t1125 * t172
     #3 / 0.2D1) - 0.180D3 * t6 * t131 * (t1124 * t1723 - t1728) - t6 * 
     #t141 * t1723) * t221 * t150 / 0.720D3
      t1819 = FJET(XB1, XB2, s, -t934, 0.0D0, 0.0D0, -t996, t997, t1818)
      t1821 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, x1, t197, 0.10D1,
     # x4)
      t1823 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, x1, t197, 0.10D1,
     # x4)
      t1835 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, x1, t197, 0.10D1,
     # x4)
      t1853 = -(0.90D2 * t6 * 0.3141592653589793D1 * (t951 * t1821 - t18
     #23) + 0.180D3 * t6 * t131 * t1821) * t148 * t222 / 0.720D3 + (0.90
     #D2 * t6 * 0.3141592653589793D1 * (-t971 * t1823 + t1835 + t974 * t
     #1821 / 0.2D1) - 0.180D3 * t6 * t131 * (-t971 * t1821 + t1823) + t6
     # * t141 * t1821) * t221 * t150 / 0.720D3
      t1854 = FJET(XB1, XB2, s, -t934, t936, 0.0D0, t932, -t941, t1853)
      t1856 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1858 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1861 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1862 = t929 * t1861
      t1874 = t1009 * t1858
      t1885 = t1861 - t1009 * t1861 * t1041
      t1899 = rrqg2qgh82J4(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1951 = -(0.90D2 * t6 * 0.3141592653589793D1 * (-(t1009 * t1856 - 
     #t1017 * t929 * t1858 + t1021 * t1862 / 0.2D1) * t1041 - t1002 * t1
     #858 + t1856 + t1003 * t1861 / 0.2D1) - 0.180D3 * t6 * t131 * (-(t1
     #874 - t1017 * t1862) * t1041 + t1858 - t1002 * t1861) + t6 * t141 
     #* t1885) * t148 * t150 / 0.1440D4 + (t6 * t141 * (-t1858 + t1069 *
     # t1861) + 0.90D2 * t6 * 0.3141592653589793D1 * (-t1074 * t1858 / 0
     #.2D1 + t1069 * t1856 - t1899 + t1077 * t1861 / 0.6D1) - t6 * t172 
     #* t1861 - 0.180D3 * t6 * t131 * (-t1856 - t1074 * t1861 / 0.2D1 + 
     #t1069 * t1858)) * t150 / 0.1440D4 - (0.90D2 * t6 * 0.3141592653589
     #793D1 * (t1858 - t1109 * t1861 - (t1874 - t1103 * t1862) * t1041) 
     #- 0.180D3 * t6 * t131 * t1885) * t148 * t222 / 0.720D3 + (0.90D2 *
     # t6 * 0.3141592653589793D1 * (-t1856 - t1125 * t1861 / 0.2D1 + t11
     #24 * t1858) - 0.180D3 * t6 * t131 * (-t1858 + t1124 * t1861) - t6 
     #* t141 * t1861) * t221 * t150 / 0.720D3
      t1952 = FJET(XB1, XB2, s, -t996, 0.0D0, 0.0D0, -t934, t997, t1951)
      t1954 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t197, -t85
     #7, x4)
      t1956 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t197, -t85
     #7, x4)
      t1958 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t197, -t85
     #7, x4)
      t1966 = z * t1956
      t1974 = t884 * t1958 * t880
      t1992 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (-z * t1954 + t901
     # * t1956 - t904 * t1958 / 0.2D1) * t880 + 0.180D3 * t61 * 0.314159
     #2653589793D1 * (-t1966 + t901 * t1958) * t880 + t55 * t1974) * t14
     #8 * t221 / 0.1440D4 - (-0.90D2 * t6 * 0.3141592653589793D1 * (-t19
     #66 + t870 * t1958) * t880 - 0.180D3 * t61 * t1974) * t148 * t222 /
     # 0.720D3
      t1993 = FJET(XB1, XB2, s, -t858, 0.0D0, t856, 0.0D0, 0.0D0, t1992)
      t1995 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, x1, t197, -t857, 
     #x4)
      t1998 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, x1, t197, -t857, 
     #x4)
      t2009 = 0.90D2 * t6 * 0.3141592653589793D1 * (-t1644 * t1674 * t19
     #95 + t1678 * t1673 * t1998) - 0.180D3 * t1686 * t1678 * t1673 * t1
     #995
      t2013 = FJET(XB1, XB2, s, -t1638, -t1629, t1628, t1618, -t941, -t2
     #009 * t148 * t222 / 0.720D3)
      t2017 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, x1, t197, -t857, 
     #x4)
      t2020 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, x1, t197, -t857, 
     #x4)
      t2031 = 0.90D2 * t6 * 0.3141592653589793D1 * (-t1644 * t1674 * t20
     #17 + t1678 * t1673 * t2020) - 0.180D3 * t1686 * t1678 * t1673 * t2
     #017
      t2035 = FJET(XB1, XB2, s, -t1629, -t1638, t1618, t1628, -t941, -t2
     #031 * t148 * t222 / 0.720D3)
      rrqg2qght8s3e1 = t414 * t413 + t632 * t631 + t850 * t849 + t924 * 
     #t923 + t993 * t992 + t1145 * t1144 + t1243 * t1242 + t1284 * t1283
     # + t1502 * t1501 + t1537 * t1536 + t1578 * t1577 + t1613 * t1612 -
     # t1695 * t1691 * t1698 / 0.720D3 - t1719 * t1715 * t1698 / 0.720D3
     # + t1819 * t1818 + t1854 * t1853 + t1952 * t1951 + t1993 * t1992 -
     # t2013 * t2009 * t1698 / 0.720D3 - t2035 * t2031 * t1698 / 0.720D3

      end function



      doubleprecision function rrqg2qght8s3e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10D
     #1, x4)
      t9 = z ** 2
      t11 = 0.1D1 / t9 / z
      t12 = x3 * t11
      t13 = x4 * 0.3141592653589793D1
      t14 = Sin(t13)
      t15 = t14 ** 2
      t16 = t1 ** 2
      t17 = t16 ** 2
      t18 = t15 * t17
      t19 = -0.1D1 + x3
      t20 = 0.1D1 / t19
      t21 = t18 * t20
      t24 = log(-0.4D1 * t12 * t21)
      t25 = t24 ** 2
      t27 = cos(t13)
      t28 = x3 * z
      t30 = Sqrt(-t28 * t19)
      t34 = 0.1D1 / (-z - x3 + 0.2D1 * t27 * t30)
      t38 = log(0.4D1 * t12 * t18)
      t39 = t38 ** 2
      t41 = -t25 * z * t34 / 0.2D1 - t39 / 0.2D1
      t45 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t49 = 0.3141592653589793D1 * lh
      t56 = t24 * z * t34 + t38
      t58 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t65 = lh ** 2
      t67 = 0.3141592653589793D1 ** 2
      t69 = 0.180D3 * t65 - 0.30D2 * t67
      t70 = t69 * 0.3141592653589793D1
      t72 = t6 * t70 * t7
      t75 = -z * t34 - 0.1D1
      t78 = 0.1D1 / x3
      t81 = t11 * t15
      t82 = t81 * t17
      t84 = log(0.4D1 * t82)
      t85 = t84 * t3
      t86 = t5 * lh
      t89 = t84 ** 2
      t90 = t89 * t3
      t95 = (0.180D3 * t85 * t86 + 0.45D2 * t90 * t5 + t6 * t69) * 0.314
     #1592653589793D1
      t98 = rrqg2qgh82J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t117 = (-0.90D2 * t90 * t86 + t6 * (0.60D2 * lh * t67 - 0.28849365
     #67583026D3 - 0.120D3 * t65 * lh) - 0.15D2 * t89 * t84 * t3 * t5 - 
     #t85 * t5 * t69) * 0.3141592653589793D1
      t120 = t6 * lh
      t125 = (-0.180D3 * t120 - 0.90D2 * t85 * t5) * 0.3141592653589793D
     #1
      t128 = x2 ** 2
      t129 = x3 * t128
      t132 = log(0.4D1 * t129 * t82)
      t134 = 0.1D1 - x2
      t135 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t134, 0.10D
     #1, x4)
      t136 = t129 * t11
      t137 = -t134
      t138 = t18 * t137
      t141 = log(-0.4D1 * t136 * t138)
      t142 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t134, 0.10D
     #1, x4)
      t144 = z * t45
      t147 = log(-0.4D1 * t136 * t21)
      t148 = t147 * z
      t157 = z * t7 * t34
      t158 = -t157 - t7 + t142
      t164 = 0.1D1 / x2
      t167 = t11 * t128
      t170 = log(0.4D1 * t167 * t18)
      t172 = t170 ** 2
      t175 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t134, 0.10D
     #1, x4)
      t178 = log(-0.4D1 * t167 * t138)
      t180 = t178 ** 2
      t193 = -t142 + t7
      t199 = x1 ** 2
      t200 = x3 * t199
      t201 = t200 * t15
      t202 = t11 * t17
      t206 = log(-0.4D1 * t201 * t202 * t20)
      t207 = t206 * z
      t213 = log(0.4D1 * t200 * t82)
      t225 = 0.1D1 / x1
      t228 = t6 * 0.3141592653589793D1
      t230 = t164 * t225
      t234 = t128 * t199
      t235 = t234 * t15
      t239 = log(-0.4D1 * t235 * t202 * t137)
      t243 = log(0.4D1 * t234 * t82)
      t256 = t199 * t15
      t259 = log(0.4D1 * t256 * t202)
      t260 = t259 ** 2
      t276 = -(0.90D2 * t6 * 0.3141592653589793D1 * t7 * t41 + (0.90D2 *
     # t6 * 0.3141592653589793D1 * t45 - 0.180D3 * t6 * t49 * t7) * t56 
     #+ (0.90D2 * t6 * 0.3141592653589793D1 * t58 - 0.180D3 * t6 * t49 *
     # t45 + t72) * t75) * t78 / 0.2880D4 + t95 * t45 / 0.2880D4 + t6 * 
     #0.3141592653589793D1 * t98 / 0.32D2 + t117 * t7 / 0.2880D4 + t125 
     #* t58 / 0.2880D4 - (0.90D2 * t6 * 0.3141592653589793D1 * (-t45 + t
     #132 * t7 + t135 - t141 * t142 - (t144 - t148 * t7) * t34) - 0.180D
     #3 * t6 * t49 * t158) * t78 * t164 / 0.1440D4 + (0.90D2 * t6 * 0.31
     #41592653589793D1 * (-t170 * t45 + t172 * t7 / 0.2D1 - t175 + t178 
     #* t135 + t58 - t180 * t142 / 0.2D1) - 0.180D3 * t6 * t49 * (t45 - 
     #t170 * t7 - t135 + t178 * t142) + t6 * t70 * t193) * t164 / 0.1440
     #D4 - (0.90D2 * t6 * 0.3141592653589793D1 * (-t45 - (t144 - t207 * 
     #t7) * t34 + t213 * t7) - 0.180D3 * t6 * t49 * (-t7 - t157)) * t78 
     #* t225 / 0.1440D4 - t228 * t158 * t78 * t230 / 0.8D1 + (0.90D2 * t
     #6 * 0.3141592653589793D1 * (t45 + t239 * t142 - t243 * t7 - t135) 
     #- 0.180D3 * t6 * t49 * t193) * t164 * t225 / 0.720D3 + (0.90D2 * t
     #6 * 0.3141592653589793D1 * (t58 + t260 * t7 / 0.2D1 - t259 * t45) 
     #- 0.180D3 * t6 * t49 * (t45 - t259 * t7) + t72) * t225 / 0.1440D4
      t277 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t276)
      t279 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t284 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t293 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t301 = t6 * t70 * t279
      t309 = rrqg2qgh84J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t318 = z * t284
      t322 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t134, 0.10D
     #1, x4)
      t324 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t134, 0.10D
     #1, x4)
      t330 = z * t279 * t34
      t331 = -t330 + t322 - t279
      t341 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t134, 0.10D
     #1, x4)
      t356 = -t322 + t279
      t410 = -(0.90D2 * t6 * 0.3141592653589793D1 * t279 * t41 + (0.90D2
     # * t6 * 0.3141592653589793D1 * t284 - 0.180D3 * t6 * t49 * t279) *
     # t56 + (0.90D2 * t6 * 0.3141592653589793D1 * t293 - 0.180D3 * t6 *
     # t49 * t284 + t301) * t75) * t78 / 0.2880D4 + t95 * t284 / 0.2880D
     #4 + t6 * 0.3141592653589793D1 * t309 / 0.32D2 + t117 * t279 / 0.28
     #80D4 + t125 * t293 / 0.2880D4 - (0.90D2 * t6 * 0.3141592653589793D
     #1 * (t132 * t279 - (t318 - t148 * t279) * t34 - t141 * t322 - t284
     # + t324) - 0.180D3 * t6 * t49 * t331) * t78 * t164 / 0.1440D4 + (0
     #.90D2 * t6 * 0.3141592653589793D1 * (t172 * t279 / 0.2D1 - t341 - 
     #t170 * t284 + t178 * t324 + t293 - t180 * t322 / 0.2D1) - 0.180D3 
     #* t6 * t49 * (-t170 * t279 + t178 * t322 + t284 - t324) + t6 * t70
     # * t356) * t164 / 0.1440D4 - (0.90D2 * t6 * 0.3141592653589793D1 *
     # (-t284 - (t318 - t207 * t279) * t34 + t213 * t279) - 0.180D3 * t6
     # * t49 * (-t279 - t330)) * t78 * t225 / 0.1440D4 - t228 * t331 * t
     #78 * t230 / 0.8D1 + (0.90D2 * t6 * 0.3141592653589793D1 * (-t324 +
     # t239 * t322 + t284 - t243 * t279) - 0.180D3 * t6 * t49 * t356) * 
     #t164 * t225 / 0.720D3 + (0.90D2 * t6 * 0.3141592653589793D1 * (-t2
     #59 * t284 + t260 * t279 / 0.2D1 + t293) - 0.180D3 * t6 * t49 * (t2
     #84 - t259 * t279) + t301) * t225 / 0.1440D4
      t411 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t410)
      t413 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t418 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t427 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t435 = t6 * t70 * t413
      t443 = rrqg2qgh81J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t451 = z * t418
      t455 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t134, 0.10D
     #1, x4)
      t458 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t134, 0.10D
     #1, x4)
      t464 = z * t413 * t34
      t465 = -t413 - t464 + t455
      t476 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t134, 0.10D
     #1, x4)
      t490 = t413 - t455
      t544 = -(0.90D2 * t6 * 0.3141592653589793D1 * t413 * t41 + (0.90D2
     # * t6 * 0.3141592653589793D1 * t418 - 0.180D3 * t6 * t49 * t413) *
     # t56 + (0.90D2 * t6 * 0.3141592653589793D1 * t427 - 0.180D3 * t6 *
     # t49 * t418 + t435) * t75) * t78 / 0.2880D4 + t95 * t418 / 0.2880D
     #4 + t6 * 0.3141592653589793D1 * t443 / 0.32D2 + t117 * t413 / 0.28
     #80D4 + t125 * t427 / 0.2880D4 - (0.90D2 * t6 * 0.3141592653589793D
     #1 * (-(t451 - t148 * t413) * t34 - t418 - t141 * t455 + t132 * t41
     #3 + t458) - 0.180D3 * t6 * t49 * t465) * t78 * t164 / 0.1440D4 + (
     #0.90D2 * t6 * 0.3141592653589793D1 * (t172 * t413 / 0.2D1 + t427 -
     # t170 * t418 - t476 + t178 * t458 - t180 * t455 / 0.2D1) - 0.180D3
     # * t6 * t49 * (t418 - t170 * t413 - t458 + t178 * t455) + t6 * t70
     # * t490) * t164 / 0.1440D4 - (0.90D2 * t6 * 0.3141592653589793D1 *
     # (-t418 - (t451 - t207 * t413) * t34 + t213 * t413) - 0.180D3 * t6
     # * t49 * (-t413 - t464)) * t78 * t225 / 0.1440D4 - t228 * t465 * t
     #78 * t230 / 0.8D1 + (0.90D2 * t6 * 0.3141592653589793D1 * (-t458 -
     # t243 * t413 + t418 + t239 * t455) - 0.180D3 * t6 * t49 * t490) * 
     #t164 * t225 / 0.720D3 + (0.90D2 * t6 * 0.3141592653589793D1 * (t26
     #0 * t413 / 0.2D1 + t427 - t259 * t418) - 0.180D3 * t6 * t49 * (-t2
     #59 * t413 + t418) + t435) * t225 / 0.1440D4
      t545 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t544)
      t547 = x2 * x3
      t548 = 0.1D1 - x3 + t547
      t549 = 0.1D1 / t548
      t550 = t547 * t549
      t551 = t2 * t550
      t552 = t19 * t549
      t553 = t2 * t552
      t554 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t134, -t552
     #, x4)
      t558 = t548 ** 2
      t564 = log(0.4D1 * t129 * t81 * t17 * t137 * t19 / t558)
      t565 = t564 * z
      t566 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t134, -t552
     #, x4)
      t570 = t137 * t19
      t572 = Sqrt(t28 * t570)
      t576 = 0.1D1 / (-z - x3 + t547 + 0.2D1 * t27 * t572)
      t580 = 0.3141592653589793D1 * z
      t581 = t566 * t576
      t589 = t6 * t580
      t591 = t78 * t164 * t225
      t595 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (-z * t554 + t565 *
     # t566) * t576 - 0.180D3 * t120 * t580 * t581) * t78 * t164 / 0.144
     #0D4 - t589 * t581 * t591 / 0.8D1
      t596 = FJET(XB1, XB2, s, 0.0D0, t551, 0.0D0, -t553, 0.0D0, t595)
      t599 = t1 * x1
      t600 = x1 * z
      t601 = -z - x1 + t600
      t602 = 0.1D1 / t601
      t604 = t137 * s * t599 * t602
      t605 = -0.1D1 + x1
      t606 = t2 * t605
      t608 = x2 * s * t599
      t609 = s * t16
      t612 = x1 * t605 * t602
      t613 = t609 * t137 * t612
      t614 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, x1, t134, 0.10D1, 
     #x4)
      t619 = 0.1D1 / t9
      t620 = t15 * t619
      t622 = t17 * t602
      t623 = t605 ** 2
      t628 = log(0.4D1 * t234 * t620 * t622 * t623 * t137)
      t630 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, x1, t134, 0.10D1, 
     #x4)
      t642 = t228 * t614 * t78 * t230 / 0.8D1 + (0.90D2 * t6 * 0.3141592
     #653589793D1 * (-t628 * t614 + t630) - 0.180D3 * t6 * t49 * t614) *
     # t164 * t225 / 0.720D3
      t643 = FJET(XB1, XB2, s, 0.0D0, t604, -t606, t608, -t613, t642)
      t646 = t2 * x1 * t602
      t647 = t609 * t612
      t648 = z * t601
      t649 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t656 = log(0.4D1 * t200 * t620 * t622 * t623 * t20)
      t657 = t656 * z
      t658 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t662 = x3 * x1
      t663 = t662 * z
      t665 = 0.2D1 * t200 * z
      t666 = x1 * t9
      t667 = x3 * t9
      t668 = t667 * x1
      t669 = t200 * t9
      t670 = x3 * t601
      t672 = Sqrt(t670 * t19)
      t677 = 0.1D1 / (-t600 - t663 - t28 + t665 - t200 + t666 + t668 - t
     #669 - t9 + 0.2D1 * t27 * t672 * z)
      t681 = t619 * t17 * t602 * t623
      t684 = log(-0.4D1 * t201 * t681)
      t692 = -t648 * t658 * t677 + t658
      t706 = log(-0.4D1 * t235 * t681)
      t719 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t724 = log(-0.4D1 * t256 * t619 * t622 * t623)
      t725 = t724 ** 2
      t743 = -(0.90D2 * t6 * 0.3141592653589793D1 * (-(t648 * t649 - t65
     #7 * t601 * t658) * t677 + t649 - t684 * t658) - 0.180D3 * t6 * t49
     # * t692) * t78 * t225 / 0.1440D4 - t228 * t692 * t78 * t230 / 0.8D
     #1 + (0.90D2 * t6 * 0.3141592653589793D1 * (-t649 + t706 * t658) + 
     #0.180D3 * t6 * t49 * t658) * t164 * t225 / 0.720D3 + (0.90D2 * t6 
     #* 0.3141592653589793D1 * (-t719 - t725 * t658 / 0.2D1 + t724 * t64
     #9) - 0.180D3 * t6 * t49 * (-t649 + t724 * t658) - t6 * t70 * t658)
     # * t225 / 0.1440D4
      t744 = FJET(XB1, XB2, s, 0.0D0, -t606, -t646, 0.0D0, t647, t743)
      t746 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t747 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t760 = t747 - t648 * t747 * t677
      t786 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t802 = -(0.90D2 * t6 * 0.3141592653589793D1 * (t746 - t684 * t747 
     #- (t648 * t746 - t657 * t601 * t747) * t677) - 0.180D3 * t6 * t49 
     #* t760) * t78 * t225 / 0.1440D4 - t228 * t760 * t78 * t230 / 0.8D1
     # + (0.90D2 * t6 * 0.3141592653589793D1 * (t706 * t747 - t746) + 0.
     #180D3 * t6 * t49 * t747) * t164 * t225 / 0.720D3 + (0.90D2 * t6 * 
     #0.3141592653589793D1 * (-t725 * t747 / 0.2D1 - t786 + t724 * t746)
     # - 0.180D3 * t6 * t49 * (-t746 + t724 * t747) - t6 * t70 * t747) *
     # t225 / 0.1440D4
      t803 = FJET(XB1, XB2, s, 0.0D0, -t646, -t606, 0.0D0, t647, t802)
      t805 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t134, -t552
     #, x4)
      t807 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t134, -t552
     #, x4)
      t814 = t807 * t576
      t825 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (-z * t805 + t565 *
     # t807) * t576 - 0.180D3 * t120 * t580 * t814) * t78 * t164 / 0.144
     #0D4 - t589 * t814 * t591 / 0.8D1
      t826 = FJET(XB1, XB2, s, 0.0D0, -t553, 0.0D0, t551, 0.0D0, t825)
      t828 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t833 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t842 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t850 = t6 * t70 * t828
      t856 = rrqg2qgh83J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t864 = z * t833
      t868 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t134, 0.10D
     #1, x4)
      t870 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t134, 0.10D
     #1, x4)
      t877 = z * t828 * t34
      t878 = -t828 + t870 - t877
      t889 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t134, 0.10D
     #1, x4)
      t903 = -t870 + t828
      t959 = -(0.90D2 * t6 * 0.3141592653589793D1 * t828 * t41 + (0.90D2
     # * t6 * 0.3141592653589793D1 * t833 - 0.180D3 * t6 * t49 * t828) *
     # t56 + (0.90D2 * t6 * 0.3141592653589793D1 * t842 - 0.180D3 * t6 *
     # t49 * t833 + t850) * t75) * t78 / 0.2880D4 + t6 * 0.3141592653589
     #793D1 * t856 / 0.32D2 + t125 * t842 / 0.2880D4 + t95 * t833 / 0.28
     #80D4 - (0.90D2 * t6 * 0.3141592653589793D1 * (-(t864 - t148 * t828
     #) * t34 - t833 + t868 + t132 * t828 - t141 * t870) - 0.180D3 * t6 
     #* t49 * t878) * t78 * t164 / 0.1440D4 + (0.90D2 * t6 * 0.314159265
     #3589793D1 * (t178 * t868 + t842 + t172 * t828 / 0.2D1 - t889 - t17
     #0 * t833 - t180 * t870 / 0.2D1) - 0.180D3 * t6 * t49 * (-t170 * t8
     #28 - t868 + t178 * t870 + t833) + t6 * t70 * t903) * t164 / 0.1440
     #D4 + t117 * t828 / 0.2880D4 - (0.90D2 * t6 * 0.3141592653589793D1 
     #* (-(t864 - t207 * t828) * t34 - t833 + t213 * t828) - 0.180D3 * t
     #6 * t49 * (-t877 - t828)) * t78 * t225 / 0.1440D4 - t228 * t878 * 
     #t78 * t230 / 0.8D1 + (0.90D2 * t6 * 0.3141592653589793D1 * (t833 +
     # t239 * t870 - t243 * t828 - t868) - 0.180D3 * t6 * t49 * t903) * 
     #t164 * t225 / 0.720D3 + (0.90D2 * t6 * 0.3141592653589793D1 * (t84
     #2 - t259 * t833 + t260 * t828 / 0.2D1) - 0.180D3 * t6 * t49 * (t83
     #3 - t259 * t828) + t850) * t225 / 0.1440D4
      t960 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t959)
      t962 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, x1, t134, 0.10D1, 
     #x4)
      t967 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, x1, t134, 0.10D1, 
     #x4)
      t980 = t228 * t962 * t78 * t230 / 0.8D1 + (0.90D2 * t6 * 0.3141592
     #653589793D1 * (t967 - t628 * t962) - 0.180D3 * t6 * t49 * t962) * 
     #t164 * t225 / 0.720D3
      t981 = FJET(XB1, XB2, s, t608, -t606, t604, 0.0D0, -t613, t980)
      t983 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t134, -t552
     #, x4)
      t985 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t134, -t552
     #, x4)
      t992 = t985 * t576
      t1003 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (-z * t983 + t565 
     #* t985) * t576 - 0.180D3 * t120 * t580 * t992) * t78 * t164 / 0.14
     #40D4 - t589 * t992 * t591 / 0.8D1
      t1004 = FJET(XB1, XB2, s, t551, 0.0D0, -t553, 0.0D0, 0.0D0, t1003)
      t1006 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, x1, t134, 0.10D1,
     # x4)
      t1011 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, x1, t134, 0.10D1,
     # x4)
      t1024 = t228 * t1006 * t78 * t230 / 0.8D1 + (0.90D2 * t6 * 0.31415
     #92653589793D1 * (t1011 - t628 * t1006) - 0.180D3 * t6 * t49 * t100
     #6) * t164 * t225 / 0.720D3
      t1025 = FJET(XB1, XB2, s, t604, 0.0D0, t608, -t606, -t613, t1024)
      t1030 = t19 * s * t1 * t605 * t549
      t1031 = t2 * x1
      t1033 = Sqrt(-t670 * t570)
      t1034 = t27 * t1033
      t1040 = t1031 * x2 * (-x3 + t547 - z + t28 - x1 + t662 + t600 - t6
     #63 + 0.2D1 * t1034) * t602 * t549
      t1041 = t606 * t550
      t1044 = t129 * x1
      t1046 = t129 * t600
      t1050 = t1031 * (0.2D1 * t1034 * x2 + t1044 + t129 * z - x2 + t547
     # - t1046 + 0.1D1 - x3) * t602 * t549
      t1052 = x2 * t199
      t1055 = x2 * x1
      t1056 = t1055 * z
      t1064 = t1052 + t28 + t200 - t666 - t1044 - 0.2D1 * t1034 * z + t1
     #056 - t547 * z + t547 * x1 - t200 * x2 - t1055 * t9 - 0.2D1 * t105
     #2 * z + t1052 * t9
      t1077 = t600 + 0.2D1 * t1034 * t1056 + t9 + t663 - t665 - t668 + t
     #669 + t1046 - 0.2D1 * t547 * t600 + t667 * t1055 + 0.2D1 * t200 * 
     #x2 * z - t200 * t9 * x2 - 0.2D1 * t1034 * t1055
      t1079 = 0.1D1 / (t1064 + t1077)
      t1081 = t6 * 0.3141592653589793D1 * t601 * t1079
      t1082 = -z + t1056 - t1055
      t1083 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, x1, t134, -t552, 
     #x4)
      t1088 = FJET(XB1, XB2, s, t1030, t1040, -t1041, -t1050, -t613, -t1
     #081 * t1082 * t1083 * t591 / 0.8D1)
      t1091 = t5 * 0.3141592653589793D1 * t601
      t1093 = t1079 * t1082
      t1098 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, x1, t134, -t552, 
     #x4)
      t1103 = FJET(XB1, XB2, s, t1040, t1030, -t1050, -t1041, -t613, -t1
     #081 * t1082 * t1098 * t591 / 0.8D1)
      t1110 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1111 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1124 = t1111 - t648 * t1111 * t677
      t1150 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1166 = -(0.90D2 * t6 * 0.3141592653589793D1 * (t1110 - t684 * t11
     #11 - (t648 * t1110 - t657 * t601 * t1111) * t677) - 0.180D3 * t6 *
     # t49 * t1124) * t78 * t225 / 0.1440D4 - t228 * t1124 * t78 * t230 
     #/ 0.8D1 + (0.90D2 * t6 * 0.3141592653589793D1 * (t706 * t1111 - t1
     #110) + 0.180D3 * t6 * t49 * t1111) * t164 * t225 / 0.720D3 + (0.90
     #D2 * t6 * 0.3141592653589793D1 * (-t725 * t1111 / 0.2D1 - t1150 + 
     #t724 * t1110) - 0.180D3 * t6 * t49 * (-t1110 + t724 * t1111) - t6 
     #* t70 * t1111) * t225 / 0.1440D4
      t1167 = FJET(XB1, XB2, s, -t606, 0.0D0, 0.0D0, -t646, t647, t1166)
      t1169 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, x1, t134, 0.10D1,
     # x4)
      t1175 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, x1, t134, 0.10D1,
     # x4)
      t1187 = t228 * t1169 * t78 * t230 / 0.8D1 + (0.90D2 * t6 * 0.31415
     #92653589793D1 * (-t628 * t1169 + t1175) - 0.180D3 * t6 * t49 * t11
     #69) * t164 * t225 / 0.720D3
      t1188 = FJET(XB1, XB2, s, -t606, t608, 0.0D0, t604, -t613, t1187)
      t1190 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1192 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1204 = t1192 - t648 * t1192 * t677
      t1228 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D
     #1, x4)
      t1246 = -(0.90D2 * t6 * 0.3141592653589793D1 * (-(t648 * t1190 - t
     #657 * t601 * t1192) * t677 + t1190 - t684 * t1192) - 0.180D3 * t6 
     #* t49 * t1204) * t78 * t225 / 0.1440D4 - t228 * t1204 * t78 * t230
     # / 0.8D1 + (0.90D2 * t6 * 0.3141592653589793D1 * (-t1190 + t706 * 
     #t1192) + 0.180D3 * t6 * t49 * t1192) * t164 * t225 / 0.720D3 + (0.
     #90D2 * t6 * 0.3141592653589793D1 * (-t1228 - t725 * t1192 / 0.2D1 
     #+ t724 * t1190) - 0.180D3 * t6 * t49 * (-t1190 + t724 * t1192) - t
     #6 * t70 * t1192) * t225 / 0.1440D4
      t1247 = FJET(XB1, XB2, s, -t646, 0.0D0, 0.0D0, -t606, t647, t1246)
      t1249 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t134, -t55
     #2, x4)
      t1251 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t134, -t55
     #2, x4)
      t1258 = t1251 * t576
      t1269 = -(-0.90D2 * t6 * 0.3141592653589793D1 * (-z * t1249 + t565
     # * t1251) * t576 - 0.180D3 * t120 * t580 * t1258) * t78 * t164 / 0
     #.1440D4 - t589 * t1258 * t591 / 0.8D1
      t1270 = FJET(XB1, XB2, s, -t553, 0.0D0, t551, 0.0D0, 0.0D0, t1269)
      t1272 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, x1, t134, -t552, 
     #x4)
      t1277 = FJET(XB1, XB2, s, -t1050, -t1041, t1040, t1030, -t613, -t1
     #081 * t1082 * t1272 * t591 / 0.8D1)
      t1284 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, x1, t134, -t552, 
     #x4)
      t1289 = FJET(XB1, XB2, s, -t1041, -t1050, t1030, t1040, -t613, -t1
     #081 * t1082 * t1284 * t591 / 0.8D1)
      rrqg2qght8s3e0 = t277 * t276 + t411 * t410 + t545 * t544 + t596 * 
     #t595 + t643 * t642 + t744 * t743 + t803 * t802 + t826 * t825 + t96
     #0 * t959 + t981 * t980 + t1003 * t1004 + t1025 * t1024 - t1088 * t
     #3 * t1091 * t1093 * t1083 * t591 / 0.8D1 - t1103 * t3 * t1091 * t1
     #093 * t1098 * t591 / 0.8D1 + t1167 * t1166 + t1188 * t1187 + t1247
     # * t1246 + t1270 * t1269 - t1277 * t3 * t1091 * t1093 * t1272 * t5
     #91 / 0.8D1 - t1289 * t3 * t1091 * t1093 * t1284 * t591 / 0.8D1

      end function



      doubleprecision function rrqg2qght8s3em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10D
     #1, x4)
      t9 = z ** 2
      t11 = 0.1D1 / t9 / z
      t12 = x3 * t11
      t13 = x4 * 0.3141592653589793D1
      t14 = Sin(t13)
      t15 = t14 ** 2
      t16 = t1 ** 2
      t17 = t16 ** 2
      t18 = t15 * t17
      t19 = -0.1D1 + x3
      t24 = log(-0.4D1 * t12 * t18 / t19)
      t26 = cos(t13)
      t27 = x3 * z
      t29 = Sqrt(-t27 * t19)
      t33 = 0.1D1 / (-z - x3 + 0.2D1 * t26 * t29)
      t37 = log(0.4D1 * t12 * t18)
      t38 = t24 * z * t33 + t37
      t42 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t46 = 0.3141592653589793D1 * lh
      t49 = 0.180D3 * t6 * t46 * t7
      t52 = -z * t33 - 0.1D1
      t55 = 0.1D1 / x3
      t58 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t67 = log(0.4D1 * t11 * t15 * t17)
      t68 = t67 * t3
      t72 = (-0.180D3 * t6 * lh - 0.90D2 * t68 * t5) * 0.314159265358979
     #3D1
      t78 = t67 ** 2
      t82 = lh ** 2
      t84 = 0.3141592653589793D1 ** 2
      t89 = (0.180D3 * t68 * t5 * lh + 0.45D2 * t78 * t3 * t5 + t6 * (0.
     #180D3 * t82 - 0.30D2 * t84)) * 0.3141592653589793D1
      t92 = t6 * 0.3141592653589793D1
      t94 = z * t7 * t33
      t95 = 0.1D1 - x2
      t96 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t95, 0.10D1,
     # x4)
      t99 = 0.1D1 / x2
      t103 = x2 ** 2
      t104 = t11 * t103
      t107 = log(0.4D1 * t104 * t18)
      t109 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t95, 0.10D1
     #, x4)
      t110 = -t95
      t114 = log(-0.4D1 * t104 * t18 * t110)
      t120 = -t96 + t7
      t128 = 0.1D1 / x1
      t132 = x1 ** 2
      t133 = t132 * t15
      t137 = log(0.4D1 * t133 * t11 * t17)
      t151 = -(0.90D2 * t6 * 0.3141592653589793D1 * t7 * t38 + (0.90D2 *
     # t6 * 0.3141592653589793D1 * t42 - t49) * t52) * t55 / 0.2880D4 + 
     #t6 * 0.3141592653589793D1 * t58 / 0.32D2 + t72 * t42 / 0.2880D4 + 
     #t89 * t7 / 0.2880D4 - t92 * (-t94 - t7 + t96) * t55 * t99 / 0.16D2
     # + (0.90D2 * t6 * 0.3141592653589793D1 * (t42 - t107 * t7 - t109 +
     # t114 * t96) - 0.180D3 * t6 * t46 * t120) * t99 / 0.1440D4 + t92 *
     # t120 * t99 * t128 / 0.8D1 + (0.90D2 * t6 * 0.3141592653589793D1 *
     # (t42 - t137 * t7) - t49) * t128 / 0.1440D4 - t92 * (-t7 - t94) * 
     #t55 * t128 / 0.16D2
      t152 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t151)
      t154 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t159 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t165 = 0.180D3 * t6 * t46 * t154
      t171 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t180 = z * t154 * t33
      t181 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t95, 0.10D1
     #, x4)
      t189 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t95, 0.10D1
     #, x4)
      t194 = -t181 + t154
      t218 = -(0.90D2 * t6 * 0.3141592653589793D1 * t154 * t38 + (0.90D2
     # * t6 * 0.3141592653589793D1 * t159 - t165) * t52) * t55 / 0.2880D
     #4 + t6 * 0.3141592653589793D1 * t171 / 0.32D2 + t72 * t159 / 0.288
     #0D4 + t89 * t154 / 0.2880D4 - t92 * (-t180 + t181 - t154) * t55 * 
     #t99 / 0.16D2 + (0.90D2 * t6 * 0.3141592653589793D1 * (-t107 * t154
     # + t114 * t181 + t159 - t189) - 0.180D3 * t6 * t46 * t194) * t99 /
     # 0.1440D4 + t92 * t194 * t99 * t128 / 0.8D1 + (0.90D2 * t6 * 0.314
     #1592653589793D1 * (t159 - t137 * t154) - t165) * t128 / 0.1440D4 -
     # t92 * (-t154 - t180) * t55 * t128 / 0.16D2
      t219 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t218)
      t221 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t226 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t232 = 0.180D3 * t6 * t46 * t221
      t238 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t247 = z * t221 * t33
      t248 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t95, 0.10D1
     #, x4)
      t255 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t95, 0.10D1
     #, x4)
      t261 = t221 - t248
      t285 = -(0.90D2 * t6 * 0.3141592653589793D1 * t221 * t38 + (0.90D2
     # * t6 * 0.3141592653589793D1 * t226 - t232) * t52) * t55 / 0.2880D
     #4 + t6 * 0.3141592653589793D1 * t238 / 0.32D2 + t72 * t226 / 0.288
     #0D4 + t89 * t221 / 0.2880D4 - t92 * (-t221 - t247 + t248) * t55 * 
     #t99 / 0.16D2 + (0.90D2 * t6 * 0.3141592653589793D1 * (t226 - t107 
     #* t221 - t255 + t114 * t248) - 0.180D3 * t6 * t46 * t261) * t99 / 
     #0.1440D4 + t92 * t261 * t99 * t128 / 0.8D1 + (0.90D2 * t6 * 0.3141
     #592653589793D1 * (-t137 * t221 + t226) - t232) * t128 / 0.1440D4 -
     # t92 * (-t221 - t247) * t55 * t128 / 0.16D2
      t286 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t285)
      t288 = x2 * x3
      t290 = 0.1D1 / (0.1D1 - x3 + t288)
      t292 = t2 * t288 * t290
      t293 = t19 * t290
      t294 = t2 * t293
      t296 = t6 * 0.3141592653589793D1 * z
      t297 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t95, -t293,
     # x4)
      t300 = Sqrt(t27 * t110 * t19)
      t304 = 0.1D1 / (-z - x3 + t288 + 0.2D1 * t26 * t300)
      t306 = t55 * t99
      t310 = FJET(XB1, XB2, s, 0.0D0, t292, 0.0D0, -t294, 0.0D0, -t296 *
     # t297 * t304 * t306 / 0.16D2)
      t312 = t5 * 0.3141592653589793D1
      t316 = t304 * t55 * t99
      t321 = t1 * x1
      t322 = x1 * z
      t323 = -z - x1 + t322
      t324 = 0.1D1 / t323
      t326 = t110 * s * t321 * t324
      t327 = -0.1D1 + x1
      t328 = t2 * t327
      t330 = x2 * s * t321
      t331 = s * t16
      t334 = x1 * t327 * t324
      t335 = t331 * t110 * t334
      t336 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, x1, t95, 0.10D1, x
     #4)
      t341 = FJET(XB1, XB2, s, 0.0D0, t326, -t328, t330, -t335, t92 * t3
     #36 * t99 * t128 / 0.8D1)
      t345 = t99 * t128
      t350 = t2 * x1 * t324
      t351 = t331 * t334
      t352 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t357 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t361 = t327 ** 2
      t365 = log(-0.4D1 * t133 / t9 * t17 * t324 * t361)
      t377 = z * t323
      t380 = x3 * t132
      t389 = Sqrt(x3 * t323 * t19)
      t394 = 0.1D1 / (-t322 - x3 * x1 * z - t27 + 0.2D1 * t380 * z - t38
     #0 + x1 * t9 + x3 * t9 * x1 - t380 * t9 - t9 + 0.2D1 * t26 * t389 *
     # z)
      t402 = -t92 * t352 * t99 * t128 / 0.8D1 + (0.90D2 * t6 * 0.3141592
     #653589793D1 * (-t357 + t365 * t352) + 0.180D3 * t6 * t46 * t352) *
     # t128 / 0.1440D4 - t92 * (-t377 * t352 * t394 + t352) * t55 * t128
     # / 0.16D2
      t403 = FJET(XB1, XB2, s, 0.0D0, -t328, -t350, 0.0D0, t351, t402)
      t405 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t410 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t429 = -t92 * t405 * t99 * t128 / 0.8D1 + (0.90D2 * t6 * 0.3141592
     #653589793D1 * (-t410 + t365 * t405) + 0.180D3 * t6 * t46 * t405) *
     # t128 / 0.1440D4 - t92 * (t405 - t377 * t405 * t394) * t55 * t128 
     #/ 0.16D2
      t430 = FJET(XB1, XB2, s, 0.0D0, -t350, -t328, 0.0D0, t351, t429)
      t432 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t95, -t293,
     # x4)
      t437 = FJET(XB1, XB2, s, 0.0D0, -t294, 0.0D0, t292, 0.0D0, -t296 *
     # t432 * t304 * t306 / 0.16D2)
      t444 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t448 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t455 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t461 = 0.180D3 * t6 * t46 * t448
      t467 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t95, 0.10D1
     #, x4)
      t469 = z * t448 * t33
      t476 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t95, 0.10D1
     #, x4)
      t482 = -t467 + t448
      t508 = t6 * 0.3141592653589793D1 * t444 / 0.32D2 + t89 * t448 / 0.
     #2880D4 - (0.90D2 * t6 * 0.3141592653589793D1 * t448 * t38 + (0.90D
     #2 * t6 * 0.3141592653589793D1 * t455 - t461) * t52) * t55 / 0.2880
     #D4 - t92 * (-t448 + t467 - t469) * t55 * t99 / 0.16D2 + (0.90D2 * 
     #t6 * 0.3141592653589793D1 * (-t107 * t448 - t476 + t114 * t467 + t
     #455) - 0.180D3 * t6 * t46 * t482) * t99 / 0.1440D4 + t72 * t455 / 
     #0.2880D4 + t92 * t482 * t99 * t128 / 0.8D1 + (0.90D2 * t6 * 0.3141
     #592653589793D1 * (t455 - t137 * t448) - t461) * t128 / 0.1440D4 - 
     #t92 * (-t469 - t448) * t55 * t128 / 0.16D2
      t509 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t508)
      t511 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, x1, t95, 0.10D1, x
     #4)
      t516 = FJET(XB1, XB2, s, t330, -t328, t326, 0.0D0, -t335, t92 * t5
     #11 * t99 * t128 / 0.8D1)
      t523 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t95, -t293,
     # x4)
      t528 = FJET(XB1, XB2, s, t292, 0.0D0, -t294, 0.0D0, 0.0D0, -t296 *
     # t523 * t304 * t306 / 0.16D2)
      t535 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, x1, t95, 0.10D1, x
     #4)
      t540 = FJET(XB1, XB2, s, t326, 0.0D0, t330, -t328, -t335, t92 * t5
     #35 * t99 * t128 / 0.8D1)
      t547 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t552 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t571 = -t92 * t547 * t99 * t128 / 0.8D1 + (0.90D2 * t6 * 0.3141592
     #653589793D1 * (-t552 + t365 * t547) + 0.180D3 * t6 * t46 * t547) *
     # t128 / 0.1440D4 - t92 * (t547 - t377 * t547 * t394) * t55 * t128 
     #/ 0.16D2
      t572 = FJET(XB1, XB2, s, -t328, 0.0D0, 0.0D0, -t350, t351, t571)
      t574 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, x1, t95, 0.10D1, x
     #4)
      t579 = FJET(XB1, XB2, s, -t328, t330, 0.0D0, t326, -t335, t92 * t5
     #74 * t99 * t128 / 0.8D1)
      t586 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t591 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t610 = -t92 * t586 * t99 * t128 / 0.8D1 + (0.90D2 * t6 * 0.3141592
     #653589793D1 * (-t591 + t365 * t586) + 0.180D3 * t6 * t46 * t586) *
     # t128 / 0.1440D4 - t92 * (t586 - t377 * t586 * t394) * t55 * t128 
     #/ 0.16D2
      t611 = FJET(XB1, XB2, s, -t350, 0.0D0, 0.0D0, -t328, t351, t610)
      t613 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t95, -t293,
     # x4)
      t618 = FJET(XB1, XB2, s, -t294, 0.0D0, t292, 0.0D0, 0.0D0, -t296 *
     # t613 * t304 * t306 / 0.16D2)
      rrqg2qght8s3em1 = t152 * t151 + t219 * t218 + t286 * t285 - t310 *
     # t3 * t312 * z * t297 * t316 / 0.16D2 + t341 * t3 * t5 * 0.3141592
     #653589793D1 * t336 * t345 / 0.8D1 + t403 * t402 + t430 * t429 - t4
     #37 * t3 * t312 * z * t432 * t316 / 0.16D2 + t509 * t508 + t516 * t
     #3 * t5 * 0.3141592653589793D1 * t511 * t345 / 0.8D1 - t528 * t3 * 
     #t312 * z * t523 * t316 / 0.16D2 + t540 * t3 * t5 * 0.3141592653589
     #793D1 * t535 * t345 / 0.8D1 + t572 * t571 + t579 * t3 * t5 * 0.314
     #1592653589793D1 * t574 * t345 / 0.8D1 + t611 * t610 - t618 * t3 * 
     #t312 * z * t613 * t316 / 0.16D2

      end function



      doubleprecision function rrqg2qght8s3em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = t6 * 0.3141592653589793D1
      t8 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10D
     #1, x4)
      t9 = x4 * 0.3141592653589793D1
      t10 = cos(t9)
      t14 = Sqrt(-x3 * z * (-0.1D1 + x3))
      t20 = -z / (-z - x3 + 0.2D1 * t10 * t14) - 0.1D1
      t22 = 0.1D1 / x3
      t26 = 0.1D1 - x2
      t27 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t26, 0.10D1,
     # x4)
      t30 = 0.1D1 / x2
      t35 = 0.1D1 / x1
      t39 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t45 = z ** 2
      t48 = Sin(t9)
      t49 = t48 ** 2
      t51 = t1 ** 2
      t52 = t51 ** 2
      t55 = log(0.4D1 / t45 / z * t49 * t52)
      t60 = (-0.180D3 * t6 * lh - 0.90D2 * t55 * t3 * t5) * 0.3141592653
     #589793D1
      t63 = -t7 * t8 * t20 * t22 / 0.32D2 + t6 * 0.3141592653589793D1 * 
     #(-t27 + t8) * t30 / 0.16D2 + t6 * 0.3141592653589793D1 * t8 * t35 
     #/ 0.16D2 + t6 * 0.3141592653589793D1 * t39 / 0.32D2 + t60 * t8 / 0
     #.2880D4
      t64 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t63)
      t66 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t71 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t26, 0.10D1,
     # x4)
      t81 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t87 = -t7 * t66 * t20 * t22 / 0.32D2 + t6 * 0.3141592653589793D1 *
     # (-t71 + t66) * t30 / 0.16D2 + t6 * 0.3141592653589793D1 * t66 * t
     #35 / 0.16D2 + t6 * 0.3141592653589793D1 * t81 / 0.32D2 + t60 * t66
     # / 0.2880D4
      t88 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t87)
      t90 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t95 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t26, 0.10D1,
     # x4)
      t105 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t111 = -t7 * t90 * t20 * t22 / 0.32D2 + t6 * 0.3141592653589793D1 
     #* (t90 - t95) * t30 / 0.16D2 + t6 * 0.3141592653589793D1 * t90 * t
     #35 / 0.16D2 + t6 * 0.3141592653589793D1 * t105 / 0.32D2 + t60 * t9
     #0 / 0.2880D4
      t112 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t111)
      t114 = -0.1D1 + x1
      t115 = t2 * t114
      t118 = 0.1D1 / (-z - x1 + x1 * z)
      t120 = t2 * x1 * t118
      t124 = s * t51 * x1 * t114 * t118
      t125 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t127 = 0.3141592653589793D1 * t125 * t35
      t130 = FJET(XB1, XB2, s, 0.0D0, -t115, -t120, 0.0D0, t124, -t6 * t
     #127 / 0.16D2)
      t135 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t137 = 0.3141592653589793D1 * t135 * t35
      t140 = FJET(XB1, XB2, s, 0.0D0, -t120, -t115, 0.0D0, t124, -t6 * t
     #137 / 0.16D2)
      t145 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t150 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t26, 0.10D1
     #, x4)
      t160 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.1
     #0D1, x4)
      t166 = -t7 * t145 * t20 * t22 / 0.32D2 + t6 * 0.3141592653589793D1
     # * (-t150 + t145) * t30 / 0.16D2 + t6 * 0.3141592653589793D1 * t14
     #5 * t35 / 0.16D2 + t6 * 0.3141592653589793D1 * t160 / 0.32D2 + t60
     # * t145 / 0.2880D4
      t167 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t166)
      t169 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t171 = 0.3141592653589793D1 * t169 * t35
      t174 = FJET(XB1, XB2, s, -t115, 0.0D0, 0.0D0, -t120, t124, -t6 * t
     #171 / 0.16D2)
      t179 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0.10D1
     #, x4)
      t181 = 0.3141592653589793D1 * t179 * t35
      t184 = FJET(XB1, XB2, s, -t120, 0.0D0, 0.0D0, -t115, t124, -t6 * t
     #181 / 0.16D2)
      rrqg2qght8s3em2 = t64 * t63 + t88 * t87 + t112 * t111 - t130 * t3 
     #* t5 * t127 / 0.16D2 - t140 * t3 * t5 * t137 / 0.16D2 + t167 * t16
     #6 - t174 * t3 * t5 * t171 / 0.16D2 - t184 * t3 * t5 * t181 / 0.16D
     #2

      end function



      doubleprecision function rrqg2qght8s3em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10D
     #1, x4)
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t6 * 0.314
     #1592653589793D1 * t7 / 0.32D2)
      t13 = t5 * 0.3141592653589793D1
      t16 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t20 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t6 * 0.314
     #1592653589793D1 * t16 / 0.32D2)
      t24 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t28 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t6 * 0.314
     #1592653589793D1 * t24 / 0.32D2)
      t32 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1, 0.10
     #D1, x4)
      t36 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t6 * 0.314
     #1592653589793D1 * t32 / 0.32D2)
      rrqg2qght8s3em3 = t11 * t3 * t13 * t7 / 0.32D2 + t20 * t3 * t13 * 
     #t16 / 0.32D2 + t28 * t3 * t13 * t24 / 0.32D2 + t36 * t3 * t13 * t3
     #2 / 0.32D2

      end function



      doubleprecision function rrqg2qght8s3em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6
      rrqg2qght8s3em4 = 0.0D0

      end function


      doubleprecision function rrqg2qght8s4e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x1
      t4 = -0.1D1 + x1
      t5 = t2 * t4
      t6 = 0.1D1 / t1
      t7 = s ** 2
      t8 = 0.1D1 / t7
      t9 = t6 * t8
      t10 = x1 ** 2
      t11 = x3 * t10
      t12 = x4 * 0.3141592653589793D1
      t13 = Sin(t12)
      t14 = t13 ** 2
      t16 = z ** 2
      t17 = 0.1D1 / t16
      t18 = t1 ** 2
      t19 = t18 ** 2
      t21 = x1 * z
      t22 = -z - x1 + t21
      t23 = 0.1D1 / t22
      t24 = t4 ** 2
      t25 = t23 * t24
      t29 = log(-0.4D1 * t11 * t14 * t17 * t19 * t25)
      t30 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1, 
     #x4)
      t32 = t29 ** 2
      t33 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1, 
     #x4)
      t36 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1, 
     #x4)
      t41 = 0.3141592653589793D1 * lh
      t47 = lh ** 2
      t49 = 0.3141592653589793D1 ** 2
      t51 = 0.180D3 * t47 - 0.30D2 * t49
      t52 = t51 * 0.3141592653589793D1
      t54 = t9 * t52 * t33
      t56 = 0.1D1 / x3
      t58 = 0.1D1 / x1
      t60 = t10 * t14
      t66 = log(-0.4D1 * t60 * t17 * t19 * t23 * t24)
      t71 = rrqg2qgh82J4(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1, 
     #x4)
      t72 = t66 ** 2
      t76 = t72 * t66
      t87 = 0.60D2 * lh * t49 - 0.2884936567583026D3 - 0.120D3 * t47 * l
     #h
      t88 = t87 * 0.3141592653589793D1
      t100 = x2 * x3
      t101 = t10 * t19
      t102 = t100 * t101
      t103 = t14 * t17
      t104 = t103 * t25
      t107 = log(-0.4D1 * t102 * t104)
      t118 = 0.1D1 / x2
      t119 = t118 * t58
      t121 = x2 * t10
      t122 = t121 * t19
      t125 = log(-0.4D1 * t122 * t104)
      t126 = t125 ** 2
      t143 = -(0.90D2 * t9 * 0.3141592653589793D1 * (-t29 * t30 + t32 * 
     #t33 / 0.2D1 + t36) - 0.180D3 * t9 * t41 * (t30 - t29 * t33) + t54)
     # * t56 * t58 / 0.720D3 + (t9 * t52 * (t66 * t33 - t30) + 0.90D2 * 
     #t9 * 0.3141592653589793D1 * (-t71 - t72 * t30 / 0.2D1 + t66 * t36 
     #+ t76 * t33 / 0.6D1) - t9 * t88 * t33 - 0.180D3 * t9 * t41 * (-t36
     # + t66 * t30 - t72 * t33 / 0.2D1)) * t58 / 0.720D3 + (0.90D2 * t9 
     #* 0.3141592653589793D1 * (t107 * t33 - t30) + 0.180D3 * t9 * t41 *
     # t33) * t56 * t119 / 0.720D3 - (0.90D2 * t9 * 0.3141592653589793D1
     # * (t36 + t126 * t33 / 0.2D1 - t125 * t30) - 0.180D3 * t9 * t41 * 
     #(t30 - t125 * t33) + t54) * t118 * t58 / 0.720D3
      t144 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t143)
      t146 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t148 = 0.1D1 / t16 / z
      t149 = x3 * t148
      t150 = t14 * t19
      t153 = log(0.4D1 * t149 * t150)
      t154 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t159 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t161 = t153 ** 2
      t164 = t161 * t153
      t167 = rrqg2qgh82J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t173 = t9 * t88 * t154
      t184 = t148 * t14
      t185 = t184 * t19
      t187 = log(0.4D1 * t185)
      t188 = t187 ** 2
      t191 = t188 * t187
      t197 = t8 * lh
      t205 = t8 * t51
      t210 = t8 * t87
      t213 = t49 ** 2
      t214 = t47 ** 2
      t220 = t8 * (t213 + 0.60D2 * t214 + 0.5769873135166051D3 * lh - 0.
     #60D2 * t47 * t49)
      t226 = rrqg2qgh82J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t228 = t188 ** 2
      t240 = log(0.4D1 * t11 * t185)
      t242 = t240 ** 2
      t260 = t148 * t19
      t263 = log(0.4D1 * t60 * t260)
      t269 = t263 ** 2
      t272 = t269 * t263
      t289 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t290 = t11 * x2
      t291 = -0.1D1 + x2
      t292 = t291 ** 2
      t297 = log(0.4D1 * t290 * t150 * t148 * t292)
      t298 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t302 = log(0.4D1 * t290 * t185)
      t308 = -t298 + t154
      t316 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t320 = log(0.4D1 * t122 * t184 * t292)
      t321 = t320 ** 2
      t326 = log(0.4D1 * t121 * t185)
      t329 = t326 ** 2
      t344 = -t9 * t52 * t308
      t349 = t100 * t148
      t350 = t150 * t292
      t353 = log(0.4D1 * t349 * t350)
      t357 = log(0.4D1 * t100 * t185)
      t359 = t353 ** 2
      t362 = t357 ** 2
      t379 = x2 * t148
      t382 = log(0.4D1 * t379 * t350)
      t386 = log(0.4D1 * t379 * t150)
      t391 = t386 ** 2
      t392 = t391 * t386
      t395 = t382 ** 2
      t399 = rrqg2qgh82J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t400 = t395 * t382
      t425 = (t9 * t52 * (t146 - t153 * t154) + 0.90D2 * t9 * 0.31415926
     #53589793D1 * (-t153 * t159 + t161 * t146 / 0.2D1 - t164 * t154 / 0
     #.6D1 + t167) + t173 - 0.180D3 * t9 * t41 * (t159 - t153 * t146 + t
     #161 * t154 / 0.2D1)) * t56 / 0.1440D4 + (-0.180D3 * (t188 * t146 /
     # 0.2D1 - t191 * t154 / 0.6D1 + t167 - t187 * t159) * t6 * t197 + (
     #t159 - t187 * t146 + t188 * t154 / 0.2D1) * t6 * t205 + (t146 - t1
     #87 * t154) * t6 * t210 + t154 * t6 * t220 + 0.90D2 * (-t191 * t146
     # / 0.6D1 + t188 * t159 / 0.2D1 + t226 - t187 * t167 + t228 * t154 
     #/ 0.24D2) * t6 * t8) * 0.3141592653589793D1 / 0.1440D4 - (0.90D2 *
     # t9 * 0.3141592653589793D1 * (t240 * t146 - t242 * t154 / 0.2D1 - 
     #t159) - 0.180D3 * t9 * t41 * (t240 * t154 - t146) - t9 * t52 * t15
     #4) * t56 * t58 / 0.720D3 + (t9 * t52 * (t146 - t263 * t154) + 0.90
     #D2 * t9 * 0.3141592653589793D1 * (-t263 * t159 + t269 * t146 / 0.2
     #D1 - t272 * t154 / 0.6D1 + t167) + t173 - 0.180D3 * t9 * t41 * (t1
     #59 - t263 * t146 + t269 * t154 / 0.2D1)) * t58 / 0.720D3 + (0.90D2
     # * t9 * 0.3141592653589793D1 * (-t289 + t297 * t298 - t302 * t154 
     #+ t146) - 0.180D3 * t9 * t41 * t308) * t56 * t119 / 0.720D3 - (0.9
     #0D2 * t9 * 0.3141592653589793D1 * (t316 + t321 * t298 / 0.2D1 - t1
     #59 + t326 * t146 - t320 * t289 - t329 * t154 / 0.2D1) - 0.180D3 * 
     #t9 * t41 * (-t320 * t298 - t146 + t289 + t326 * t154) + t344) * t1
     #18 * t58 / 0.720D3 - (0.90D2 * t9 * 0.3141592653589793D1 * (-t159 
     #+ t316 - t353 * t289 + t357 * t146 + t359 * t298 / 0.2D1 - t362 * 
     #t154 / 0.2D1) - 0.180D3 * t9 * t41 * (-t353 * t298 - t146 + t289 +
     # t357 * t154) + t344) * t56 * t118 / 0.1440D4 + (t9 * t52 * (t382 
     #* t298 - t289 + t146 - t386 * t154) + 0.90D2 * t9 * 0.314159265358
     #9793D1 * (-t392 * t154 / 0.6D1 - t395 * t289 / 0.2D1 - t386 * t159
     # - t399 + t167 + t400 * t298 / 0.6D1 + t391 * t146 / 0.2D1 + t382 
     #* t316) + t9 * t88 * t308 - 0.180D3 * t9 * t41 * (t382 * t289 - t3
     #95 * t298 / 0.2D1 + t159 - t386 * t146 - t316 + t391 * t154 / 0.2D
     #1)) * t118 / 0.1440D4
      t426 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t425)
      t429 = -0.1D1 + t100
      t430 = 0.1D1 / t429
      t431 = x3 * t291 * t430
      t432 = t5 * t431
      t433 = x3 * x1
      t434 = t433 * z
      t435 = t100 * z
      t437 = t100 * x1
      t439 = x2 ** 2
      t440 = t439 * x3
      t441 = t440 * x1
      t445 = t440 * t21
      t446 = x3 * z
      t447 = cos(t12)
      t449 = -0.1D1 + x3
      t450 = x2 * t449
      t452 = Sqrt(x3 * t22 * t450)
      t453 = t447 * t452
      t454 = 0.2D1 * t453
      t457 = t434 + 0.2D1 * t435 + 0.2D1 * t437 - t441 - t440 * z + t100
     # - 0.2D1 * t100 * t21 + t445 - x2 - t446 - t433 - t454 + 0.2D1 * t
     #453 * x2
      t460 = t3 * t457 * t23 * t430
      t461 = t449 * s
      t462 = t1 * t4
      t464 = t461 * t462 * t430
      t469 = t3 * t291 * (-t100 - z + t446 - x1 + t433 + t21 - t434 + t4
     #54) * t23 * t430
      t474 = s * t18 * x2 * x1 * t4 * t23
      t475 = x2 * x1
      t476 = t475 * z
      t477 = z + x1 + t476 - t475 - t21
      t478 = t22 * t477
      t479 = t449 * t430
      t480 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t479, x4)
      t487 = t429 ** 2
      t488 = 0.1D1 / t487
      t493 = log(0.4D1 * t100 * t101 * t14 * t17 * t24 * t23 * t292 * t4
     #49 * t488)
      t494 = t493 * t22
      t495 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t479, x4)
      t515 = -0.2D1 * t453 * t475 - 0.2D1 * t453 * t21 + 0.2D1 * t453 * 
     #z + 0.2D1 * t453 * x1 + t476 - t435 - t437 - t290 + t441 + x3 * t1
     #6 * t475 + 0.2D1 * t11 * x2 * z - t11 * t16 * x2
      t528 = -t445 - t16 - 0.2D1 * t21 + 0.2D1 * t453 * t476 + 0.2D1 * x
     #1 * t16 - t10 + 0.2D1 * t10 * z - t10 * t16 + t121 - t475 * t16 - 
     #0.2D1 * t121 * z + t121 * t16
      t530 = 0.1D1 / (t515 + t528)
      t534 = t9 * t41
      t539 = -0.90D2 * t9 * 0.3141592653589793D1 * (t478 * t480 - t494 *
     # t477 * t495) * t530 + 0.180D3 * t534 * t478 * t495 * t530
      t543 = FJET(XB1, XB2, s, -t432, -t460, -t464, t469, t474, t539 * t
     #56 * t119 / 0.720D3)
      t546 = t56 * t118 * t58
      t549 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t550 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t555 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t561 = rrqg2qgh84J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t567 = t9 * t88 * t550
      t599 = rrqg2qgh84J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t656 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t658 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t663 = -t656 + t550
      t674 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t690 = -t9 * t52 * t663
      t720 = rrqg2qgh84J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t750 = (t9 * t52 * (t549 - t153 * t550) + 0.90D2 * t9 * 0.31415926
     #53589793D1 * (-t153 * t555 - t164 * t550 / 0.6D1 + t161 * t549 / 0
     #.2D1 + t561) + t567 - 0.180D3 * t9 * t41 * (t555 - t153 * t549 + t
     #161 * t550 / 0.2D1)) * t56 / 0.1440D4 + (-0.180D3 * (t188 * t549 /
     # 0.2D1 - t187 * t555 + t561 - t191 * t550 / 0.6D1) * t6 * t197 + (
     #t555 - t187 * t549 + t188 * t550 / 0.2D1) * t6 * t205 + (t549 - t1
     #87 * t550) * t6 * t210 + t550 * t6 * t220 + 0.90D2 * (t599 - t187 
     #* t561 + t188 * t555 / 0.2D1 - t191 * t549 / 0.6D1 + t228 * t550 /
     # 0.24D2) * t6 * t8) * 0.3141592653589793D1 / 0.1440D4 - (0.90D2 * 
     #t9 * 0.3141592653589793D1 * (-t242 * t550 / 0.2D1 + t240 * t549 - 
     #t555) - 0.180D3 * t9 * t41 * (-t549 + t240 * t550) - t9 * t52 * t5
     #50) * t56 * t58 / 0.720D3 + (t9 * t52 * (t549 - t263 * t550) + 0.9
     #0D2 * t9 * 0.3141592653589793D1 * (-t263 * t555 - t272 * t550 / 0.
     #6D1 + t269 * t549 / 0.2D1 + t561) + t567 - 0.180D3 * t9 * t41 * (t
     #555 - t263 * t549 + t269 * t550 / 0.2D1)) * t58 / 0.720D3 + (0.90D
     #2 * t9 * 0.3141592653589793D1 * (-t302 * t550 + t297 * t656 + t549
     # - t658) - 0.180D3 * t9 * t41 * t663) * t56 * t119 / 0.720D3 - (0.
     #90D2 * t9 * 0.3141592653589793D1 * (t321 * t656 / 0.2D1 - t555 - t
     #320 * t658 + t674 + t326 * t549 - t329 * t550 / 0.2D1) - 0.180D3 *
     # t9 * t41 * (t658 - t549 - t320 * t656 + t326 * t550) + t690) * t1
     #18 * t58 / 0.720D3 - (0.90D2 * t9 * 0.3141592653589793D1 * (-t362 
     #* t550 / 0.2D1 + t359 * t656 / 0.2D1 + t357 * t549 + t674 - t353 *
     # t658 - t555) - 0.180D3 * t9 * t41 * (-t353 * t656 - t549 + t357 *
     # t550 + t658) + t690) * t56 * t118 / 0.1440D4 + (t9 * t52 * (-t658
     # + t382 * t656 + t549 - t386 * t550) + 0.90D2 * t9 * 0.31415926535
     #89793D1 * (-t720 - t392 * t550 / 0.6D1 - t386 * t555 - t395 * t658
     # / 0.2D1 + t391 * t549 / 0.2D1 + t400 * t656 / 0.6D1 + t561 + t382
     # * t674) + t9 * t88 * t663 - 0.180D3 * t9 * t41 * (t391 * t550 / 0
     #.2D1 - t674 - t395 * t656 / 0.2D1 + t382 * t658 - t386 * t549 + t5
     #55)) * t118 / 0.1440D4
      t751 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t750)
      t753 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t754 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t761 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t763 = rrqg2qgh83J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t771 = t9 * t88 * t754
      t824 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t825 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t831 = -t825 + t754
      t840 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t858 = -t9 * t52 * t831
      t896 = rrqg2qgh83J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t941 = rrqg2qgh83J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t954 = (t9 * t52 * (t753 - t153 * t754) + 0.90D2 * t9 * 0.31415926
     #53589793D1 * (t161 * t753 / 0.2D1 - t153 * t761 + t763 - t164 * t7
     #54 / 0.6D1) + t771 - 0.180D3 * t9 * t41 * (t161 * t754 / 0.2D1 + t
     #761 - t153 * t753)) * t56 / 0.1440D4 - (0.90D2 * t9 * 0.3141592653
     #589793D1 * (-t242 * t754 / 0.2D1 + t240 * t753 - t761) - 0.180D3 *
     # t9 * t41 * (-t753 + t240 * t754) - t9 * t52 * t754) * t56 * t58 /
     # 0.720D3 + (t9 * t52 * (-t263 * t754 + t753) + 0.90D2 * t9 * 0.314
     #1592653589793D1 * (t763 - t272 * t754 / 0.6D1 - t263 * t761 + t269
     # * t753 / 0.2D1) + t771 - 0.180D3 * t9 * t41 * (t761 - t263 * t753
     # + t269 * t754 / 0.2D1)) * t58 / 0.720D3 + (0.90D2 * t9 * 0.314159
     #2653589793D1 * (-t302 * t754 - t824 + t297 * t825 + t753) - 0.180D
     #3 * t9 * t41 * t831) * t56 * t119 / 0.720D3 - (0.90D2 * t9 * 0.314
     #1592653589793D1 * (-t320 * t824 + t840 + t321 * t825 / 0.2D1 - t32
     #9 * t754 / 0.2D1 - t761 + t326 * t753) - 0.180D3 * t9 * t41 * (t82
     #4 - t320 * t825 + t326 * t754 - t753) + t858) * t118 * t58 / 0.720
     #D3 - (0.90D2 * t9 * 0.3141592653589793D1 * (t357 * t753 + t359 * t
     #825 / 0.2D1 - t353 * t824 - t362 * t754 / 0.2D1 + t840 - t761) - 0
     #.180D3 * t9 * t41 * (t824 - t753 + t357 * t754 - t353 * t825) + t8
     #58) * t56 * t118 / 0.1440D4 + (t9 * t52 * (-t824 + t753 + t382 * t
     #825 - t386 * t754) + 0.90D2 * t9 * 0.3141592653589793D1 * (t391 * 
     #t753 / 0.2D1 - t395 * t824 / 0.2D1 + t400 * t825 / 0.6D1 - t392 * 
     #t754 / 0.6D1 - t896 + t382 * t840 + t763 - t386 * t761) + t9 * t88
     # * t831 - 0.180D3 * t9 * t41 * (-t840 + t391 * t754 / 0.2D1 - t386
     # * t753 + t382 * t824 - t395 * t825 / 0.2D1 + t761)) * t118 / 0.14
     #40D4 + (-0.180D3 * (t188 * t753 / 0.2D1 - t191 * t754 / 0.6D1 - t1
     #87 * t761 + t763) * t6 * t197 + (-t187 * t753 + t188 * t754 / 0.2D
     #1 + t761) * t6 * t205 + (t753 - t187 * t754) * t6 * t210 + t754 * 
     #t6 * t220 + 0.90D2 * (-t191 * t753 / 0.6D1 + t941 - t187 * t763 + 
     #t188 * t761 / 0.2D1 + t228 * t754 / 0.24D2) * t6 * t8) * 0.3141592
     #653589793D1 / 0.1440D4
      t955 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t954)
      t957 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t958 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t961 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t973 = t9 * t52 * t958
      t986 = rrqg2qgh81J4(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t1029 = -(0.90D2 * t9 * 0.3141592653589793D1 * (t957 + t32 * t958 
     #/ 0.2D1 - t29 * t961) - 0.180D3 * t9 * t41 * (t961 - t29 * t958) +
     # t973) * t56 * t58 / 0.720D3 + (t9 * t52 * (t66 * t958 - t961) + 0
     #.90D2 * t9 * 0.3141592653589793D1 * (t66 * t957 + t76 * t958 / 0.6
     #D1 - t72 * t961 / 0.2D1 - t986) - t9 * t88 * t958 - 0.180D3 * t9 *
     # t41 * (-t957 - t72 * t958 / 0.2D1 + t66 * t961)) * t58 / 0.720D3 
     #+ (0.90D2 * t9 * 0.3141592653589793D1 * (t107 * t958 - t961) + 0.1
     #80D3 * t9 * t41 * t958) * t56 * t119 / 0.720D3 - (0.90D2 * t9 * 0.
     #3141592653589793D1 * (t126 * t958 / 0.2D1 - t125 * t961 + t957) - 
     #0.180D3 * t9 * t41 * (-t125 * t958 + t961) + t973) * t118 * t58 / 
     #0.720D3
      t1030 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t1029)
      t1032 = t2 * t479
      t1033 = t2 * t431
      t1034 = t9 * 0.3141592653589793D1
      t1035 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t479, 
     #x4)
      t1036 = t100 * t260
      t1037 = t14 * t292
      t1038 = t488 * t449
      t1043 = log(-0.4D1 * t1036 * t1037 * t1038 * t10)
      t1044 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t479, 
     #x4)
      t1049 = Sqrt(-t446 * t450)
      t1053 = 0.1D1 / (-z - t100 + 0.2D1 * t447 * t1049)
      t1057 = t9 * lh
      t1059 = z * t1053
      t1060 = 0.3141592653589793D1 * t1044 * t1059
      t1070 = log(-0.4D1 * t1036 * t1037 * t1038)
      t1072 = t1070 ** 2
      t1075 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t479, 
     #x4)
      t1087 = t9 * t51
      t1093 = (-0.90D2 * t1034 * (t1035 - t1043 * t1044) * z * t1053 + 0
     #.180D3 * t1057 * t1060) * t56 * t119 / 0.720D3 - (0.90D2 * t1034 *
     # (-t1070 * t1035 + t1072 * t1044 / 0.2D1 + t1075) * z * t1053 - 0.
     #180D3 * t1057 * 0.3141592653589793D1 * (t1035 - t1070 * t1044) * t
     #1059 + t1087 * t1060) * t56 * t118 / 0.1440D4
      t1094 = FJET(XB1, XB2, s, t1032, 0.0D0, t1033, 0.0D0, 0.0D0, t1093
     #)
      t1097 = t1 * x1
      t1098 = t291 * s * t1097
      t1100 = t2 * t475 * t23
      t1101 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x
     #4)
      t1106 = log(-0.4D1 * t102 * t103 * t25 * t292)
      t1107 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x
     #4)
      t1120 = t17 * t23
      t1125 = log(-0.4D1 * t121 * t150 * t1120 * t24 * t292)
      t1126 = t1125 ** 2
      t1129 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x
     #4)
      t1146 = (0.90D2 * t9 * 0.3141592653589793D1 * (t1101 - t1106 * t11
     #07) - 0.180D3 * t9 * t41 * t1107) * t56 * t119 / 0.720D3 - (0.90D2
     # * t9 * 0.3141592653589793D1 * (-t1126 * t1107 / 0.2D1 - t1129 + t
     #1125 * t1101) - 0.180D3 * t9 * t41 * (t1125 * t1107 - t1101) - t9 
     #* t52 * t1107) * t118 * t58 / 0.720D3
      t1147 = FJET(XB1, XB2, s, -t5, -t1098, 0.0D0, -t1100, t474, t1146)
      t1149 = t2 * x3
      t1150 = t2 * t449
      t1155 = log(-0.4D1 * t11 * t19 * t184 * t449)
      t1156 = -t449
      t1157 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t11
     #56, x4)
      t1159 = t1155 ** 2
      t1160 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t11
     #56, x4)
      t1163 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t11
     #56, x4)
      t1174 = t9 * t52 * t1160
      t1183 = log(-0.4D1 * t290 * t150 * t148 * t449)
      t1196 = t150 * t449
      t1199 = log(-0.4D1 * t149 * t1196)
      t1205 = t1199 ** 2
      t1206 = t1205 * t1199
      t1209 = rrqg2qgh82J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t11
     #56, x4)
      t1230 = log(-0.4D1 * t349 * t1196)
      t1231 = t1230 ** 2
      t1248 = -(0.90D2 * t9 * 0.3141592653589793D1 * (-t1155 * t1157 + t
     #1159 * t1160 / 0.2D1 + t1163) - 0.180D3 * t9 * t41 * (t1157 - t115
     #5 * t1160) + t1174) * t56 * t58 / 0.720D3 + (0.90D2 * t9 * 0.31415
     #92653589793D1 * (-t1157 + t1183 * t1160) + 0.180D3 * t9 * t41 * t1
     #160) * t56 * t119 / 0.720D3 + (t9 * t52 * (t1199 * t1160 - t1157) 
     #+ 0.90D2 * t9 * 0.3141592653589793D1 * (t1199 * t1163 + t1206 * t1
     #160 / 0.6D1 - t1209 - t1205 * t1157 / 0.2D1) - t9 * t88 * t1160 - 
     #0.180D3 * t9 * t41 * (-t1205 * t1160 / 0.2D1 + t1199 * t1157 - t11
     #63)) * t56 / 0.1440D4 - (0.90D2 * t9 * 0.3141592653589793D1 * (t12
     #31 * t1160 / 0.2D1 - t1230 * t1157 + t1163) - 0.180D3 * t9 * t41 *
     # (t1157 - t1230 * t1160) + t1174) * t56 * t118 / 0.1440D4
      t1249 = FJET(XB1, XB2, s, 0.0D0, t1149, 0.0D0, -t1150, 0.0D0, t124
     #8)
      t1251 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t11
     #56, x4)
      t1252 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t11
     #56, x4)
      t1261 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t11
     #56, x4)
      t1263 = rrqg2qgh83J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t11
     #56, x4)
      t1293 = t9 * t52 * t1252
      t1326 = (t9 * t52 * (-t1251 + t1199 * t1252) + 0.90D2 * t9 * 0.314
     #1592653589793D1 * (-t1205 * t1251 / 0.2D1 + t1206 * t1252 / 0.6D1 
     #+ t1199 * t1261 - t1263) - t9 * t88 * t1252 - 0.180D3 * t9 * t41 *
     # (t1199 * t1251 - t1261 - t1205 * t1252 / 0.2D1)) * t56 / 0.1440D4
     # - (0.90D2 * t9 * 0.3141592653589793D1 * (t1159 * t1252 / 0.2D1 - 
     #t1155 * t1251 + t1261) - 0.180D3 * t9 * t41 * (t1251 - t1155 * t12
     #52) + t1293) * t56 * t58 / 0.720D3 + (0.90D2 * t9 * 0.314159265358
     #9793D1 * (-t1251 + t1183 * t1252) + 0.180D3 * t9 * t41 * t1252) * 
     #t56 * t119 / 0.720D3 - (0.90D2 * t9 * 0.3141592653589793D1 * (-t12
     #30 * t1251 + t1231 * t1252 / 0.2D1 + t1261) - 0.180D3 * t9 * t41 *
     # (t1251 - t1230 * t1252) + t1293) * t56 * t118 / 0.1440D4
      t1327 = FJET(XB1, XB2, s, -t1150, 0.0D0, t1149, 0.0D0, 0.0D0, t132
     #6)
      t1329 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t479, 
     #x4)
      t1330 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t479, 
     #x4)
      t1332 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t479, 
     #x4)
      t1347 = 0.3141592653589793D1 * t1332 * t1059
      t1365 = -(0.90D2 * t1034 * (t1329 - t1070 * t1330 + t1072 * t1332 
     #/ 0.2D1) * z * t1053 - 0.180D3 * t1057 * 0.3141592653589793D1 * (t
     #1330 - t1070 * t1332) * t1059 + t1087 * t1347) * t56 * t118 / 0.14
     #40D4 + (-0.90D2 * t1034 * (t1330 - t1043 * t1332) * z * t1053 + 0.
     #180D3 * t1057 * t1347) * t56 * t119 / 0.720D3
      t1366 = FJET(XB1, XB2, s, 0.0D0, t1033, 0.0D0, t1032, 0.0D0, t1365
     #)
      t1368 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x
     #4)
      t1370 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x
     #4)
      t1383 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x
     #4)
      t1400 = (0.90D2 * t9 * 0.3141592653589793D1 * (-t1106 * t1368 + t1
     #370) - 0.180D3 * t9 * t41 * t1368) * t56 * t119 / 0.720D3 - (0.90D
     #2 * t9 * 0.3141592653589793D1 * (-t1126 * t1368 / 0.2D1 - t1383 + 
     #t1125 * t1370) - 0.180D3 * t9 * t41 * (-t1370 + t1125 * t1368) - t
     #9 * t52 * t1368) * t118 * t58 / 0.720D3
      t1401 = FJET(XB1, XB2, s, 0.0D0, -t1100, -t5, -t1098, t474, t1400)
      t1403 = t2 * t433
      t1405 = x3 * s * t462
      t1406 = t461 * t1097
      t1407 = t461 * t462
      t1408 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t1156,
     # x4)
      t1414 = log(0.4D1 * t11 * t150 * t1120 * t24 * t449)
      t1415 = t1414 ** 2
      t1416 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t1156,
     # x4)
      t1419 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t1156,
     # x4)
      t1439 = log(0.4D1 * t102 * t103 * t25 * t449)
      t1452 = -(0.90D2 * t9 * 0.3141592653589793D1 * (-t1408 - t1415 * t
     #1416 / 0.2D1 + t1414 * t1419) - 0.180D3 * t9 * t41 * (t1414 * t141
     #6 - t1419) - t9 * t52 * t1416) * t56 * t58 / 0.720D3 + (0.90D2 * t
     #9 * 0.3141592653589793D1 * (t1419 - t1439 * t1416) - 0.180D3 * t9 
     #* t41 * t1416) * t56 * t119 / 0.720D3
      t1453 = FJET(XB1, XB2, s, t1403, -t1405, -t1406, t1407, 0.0D0, t14
     #52)
      t1455 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t479, 
     #x4)
      t1456 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t479, 
     #x4)
      t1458 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t479, 
     #x4)
      t1473 = 0.3141592653589793D1 * t1458 * t1059
      t1491 = -(0.90D2 * t1034 * (t1455 - t1070 * t1456 + t1072 * t1458 
     #/ 0.2D1) * z * t1053 - 0.180D3 * t1057 * 0.3141592653589793D1 * (t
     #1456 - t1070 * t1458) * t1059 + t1087 * t1473) * t56 * t118 / 0.14
     #40D4 + (-0.90D2 * t1034 * (t1456 - t1043 * t1458) * z * t1053 + 0.
     #180D3 * t1057 * t1473) * t56 * t119 / 0.720D3
      t1492 = FJET(XB1, XB2, s, 0.0D0, t1032, 0.0D0, t1033, 0.0D0, t1491
     #)
      t1494 = t144 * t143 + t426 * t425 + t543 * t539 * t546 / 0.720D3 +
     # t751 * t750 + t955 * t954 + t1030 * t1029 + t1094 * t1093 + t1147
     # * t1146 + t1249 * t1248 + t1327 * t1326 + t1366 * t1365 + t1401 *
     # t1400 + t1453 * t1452 + t1492 * t1491
      t1495 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t1156,
     # x4)
      t1496 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t1156,
     # x4)
      t1498 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t1156,
     # x4)
      t1527 = -(0.90D2 * t9 * 0.3141592653589793D1 * (-t1495 + t1414 * t
     #1496 - t1415 * t1498 / 0.2D1) - 0.180D3 * t9 * t41 * (t1414 * t149
     #8 - t1496) - t9 * t52 * t1498) * t56 * t58 / 0.720D3 + (0.90D2 * t
     #9 * 0.3141592653589793D1 * (-t1439 * t1498 + t1496) - 0.180D3 * t9
     # * t41 * t1498) * t56 * t119 / 0.720D3
      t1528 = FJET(XB1, XB2, s, t1407, -t1406, -t1405, t1403, 0.0D0, t15
     #27)
      t1530 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t1156,
     # x4)
      t1532 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t1156,
     # x4)
      t1533 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t1156,
     # x4)
      t1562 = -(0.90D2 * t9 * 0.3141592653589793D1 * (t1414 * t1530 - t1
     #532 - t1415 * t1533 / 0.2D1) - 0.180D3 * t9 * t41 * (-t1530 + t141
     #4 * t1533) - t9 * t52 * t1533) * t56 * t58 / 0.720D3 + (0.90D2 * t
     #9 * 0.3141592653589793D1 * (-t1439 * t1533 + t1530) - 0.180D3 * t9
     # * t41 * t1533) * t56 * t119 / 0.720D3
      t1563 = FJET(XB1, XB2, s, -t1406, t1407, t1403, -t1405, 0.0D0, t15
     #62)
      t1565 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t11
     #56, x4)
      t1566 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t11
     #56, x4)
      t1571 = rrqg2qgh84J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t11
     #56, x4)
      t1576 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t11
     #56, x4)
      t1607 = t9 * t52 * t1566
      t1640 = (t9 * t52 * (-t1565 + t1199 * t1566) + 0.90D2 * t9 * 0.314
     #1592653589793D1 * (-t1571 - t1205 * t1565 / 0.2D1 + t1206 * t1566 
     #/ 0.6D1 + t1199 * t1576) - t9 * t88 * t1566 - 0.180D3 * t9 * t41 *
     # (-t1205 * t1566 / 0.2D1 - t1576 + t1199 * t1565)) * t56 / 0.1440D
     #4 - (0.90D2 * t9 * 0.3141592653589793D1 * (-t1155 * t1565 + t1159 
     #* t1566 / 0.2D1 + t1576) - 0.180D3 * t9 * t41 * (-t1155 * t1566 + 
     #t1565) + t1607) * t56 * t58 / 0.720D3 + (0.90D2 * t9 * 0.314159265
     #3589793D1 * (t1183 * t1566 - t1565) + 0.180D3 * t9 * t41 * t1566) 
     #* t56 * t119 / 0.720D3 - (0.90D2 * t9 * 0.3141592653589793D1 * (t1
     #576 - t1230 * t1565 + t1231 * t1566 / 0.2D1) - 0.180D3 * t9 * t41 
     #* (t1565 - t1230 * t1566) + t1607) * t56 * t118 / 0.1440D4
      t1641 = FJET(XB1, XB2, s, t1149, 0.0D0, -t1150, 0.0D0, 0.0D0, t164
     #0)
      t1643 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1
     #, x4)
      t1644 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1
     #, x4)
      t1646 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1
     #, x4)
      t1659 = t9 * t52 * t1646
      t1667 = rrqg2qgh84J4(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1
     #, x4)
      t1715 = -(0.90D2 * t9 * 0.3141592653589793D1 * (t1643 - t29 * t164
     #4 + t32 * t1646 / 0.2D1) - 0.180D3 * t9 * t41 * (t1644 - t29 * t16
     #46) + t1659) * t56 * t58 / 0.720D3 + (t9 * t52 * (t66 * t1646 - t1
     #644) + 0.90D2 * t9 * 0.3141592653589793D1 * (-t1667 + t76 * t1646 
     #/ 0.6D1 - t72 * t1644 / 0.2D1 + t66 * t1643) - t9 * t88 * t1646 - 
     #0.180D3 * t9 * t41 * (-t72 * t1646 / 0.2D1 + t66 * t1644 - t1643))
     # * t58 / 0.720D3 + (0.90D2 * t9 * 0.3141592653589793D1 * (t107 * t
     #1646 - t1644) + 0.180D3 * t9 * t41 * t1646) * t56 * t119 / 0.720D3
     # - (0.90D2 * t9 * 0.3141592653589793D1 * (t1643 + t126 * t1646 / 0
     #.2D1 - t125 * t1644) - 0.180D3 * t9 * t41 * (-t125 * t1646 + t1644
     #) + t1659) * t118 * t58 / 0.720D3
      t1716 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t5, t3, 0.0D0, t1715)
      t1718 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t479, x4)
      t1720 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t479, x4)
      t1732 = -0.90D2 * t9 * 0.3141592653589793D1 * (t478 * t1718 - t494
     # * t477 * t1720) * t530 + 0.180D3 * t534 * t478 * t1720 * t530
      t1736 = FJET(XB1, XB2, s, -t460, -t432, t469, -t464, t474, t1732 *
     # t56 * t119 / 0.720D3)
      t1740 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t11
     #56, x4)
      t1741 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t11
     #56, x4)
      t1743 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t11
     #56, x4)
      t1756 = t9 * t52 * t1743
      t1765 = rrqg2qgh81J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t11
     #56, x4)
      t1815 = -(0.90D2 * t9 * 0.3141592653589793D1 * (t1740 - t1230 * t1
     #741 + t1231 * t1743 / 0.2D1) - 0.180D3 * t9 * t41 * (-t1230 * t174
     #3 + t1741) + t1756) * t56 * t118 / 0.1440D4 + (t9 * t52 * (-t1741 
     #+ t1199 * t1743) + 0.90D2 * t9 * 0.3141592653589793D1 * (-t1765 - 
     #t1205 * t1741 / 0.2D1 + t1206 * t1743 / 0.6D1 + t1199 * t1740) - t
     #9 * t88 * t1743 - 0.180D3 * t9 * t41 * (t1199 * t1741 - t1205 * t1
     #743 / 0.2D1 - t1740)) * t56 / 0.1440D4 - (0.90D2 * t9 * 0.31415926
     #53589793D1 * (-t1155 * t1741 + t1159 * t1743 / 0.2D1 + t1740) - 0.
     #180D3 * t9 * t41 * (-t1155 * t1743 + t1741) + t1756) * t56 * t58 /
     # 0.720D3 + (0.90D2 * t9 * 0.3141592653589793D1 * (t1183 * t1743 - 
     #t1741) + 0.180D3 * t9 * t41 * t1743) * t56 * t119 / 0.720D3
      t1816 = FJET(XB1, XB2, s, 0.0D0, -t1150, 0.0D0, t1149, 0.0D0, t181
     #5)
      t1818 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t479, 
     #x4)
      t1819 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t479, 
     #x4)
      t1827 = 0.3141592653589793D1 * t1819 * t1059
      t1837 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t479, 
     #x4)
      t1854 = (-0.90D2 * t1034 * (t1818 - t1043 * t1819) * z * t1053 + 0
     #.180D3 * t1057 * t1827) * t56 * t119 / 0.720D3 - (0.90D2 * t1034 *
     # (-t1070 * t1818 + t1072 * t1819 / 0.2D1 + t1837) * z * t1053 - 0.
     #180D3 * t1057 * 0.3141592653589793D1 * (t1818 - t1070 * t1819) * t
     #1059 + t1087 * t1827) * t56 * t118 / 0.1440D4
      t1855 = FJET(XB1, XB2, s, t1033, 0.0D0, t1032, 0.0D0, 0.0D0, t1854
     #)
      t1857 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1
     #, x4)
      t1860 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1
     #, x4)
      t1862 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1
     #, x4)
      t1873 = t9 * t52 * t1857
      t1886 = rrqg2qgh83J4(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1
     #, x4)
      t1929 = -(0.90D2 * t9 * 0.3141592653589793D1 * (t32 * t1857 / 0.2D
     #1 - t29 * t1860 + t1862) - 0.180D3 * t9 * t41 * (-t29 * t1857 + t1
     #860) + t1873) * t56 * t58 / 0.720D3 + (t9 * t52 * (t66 * t1857 - t
     #1860) + 0.90D2 * t9 * 0.3141592653589793D1 * (t66 * t1862 + t76 * 
     #t1857 / 0.6D1 - t72 * t1860 / 0.2D1 - t1886) - t9 * t88 * t1857 - 
     #0.180D3 * t9 * t41 * (-t1862 - t72 * t1857 / 0.2D1 + t66 * t1860))
     # * t58 / 0.720D3 + (0.90D2 * t9 * 0.3141592653589793D1 * (t107 * t
     #1857 - t1860) + 0.180D3 * t9 * t41 * t1857) * t56 * t119 / 0.720D3
     # - (0.90D2 * t9 * 0.3141592653589793D1 * (-t125 * t1860 + t1862 + 
     #t126 * t1857 / 0.2D1) - 0.180D3 * t9 * t41 * (-t125 * t1857 + t186
     #0) + t1873) * t118 * t58 / 0.720D3
      t1930 = FJET(XB1, XB2, s, -t5, t3, 0.0D0, 0.0D0, 0.0D0, t1929)
      t1932 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x
     #4)
      t1934 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x
     #4)
      t1946 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x
     #4)
      t1964 = (0.90D2 * t9 * 0.3141592653589793D1 * (-t1106 * t1932 + t1
     #934) - 0.180D3 * t9 * t41 * t1932) * t56 * t119 / 0.720D3 - (0.90D
     #2 * t9 * 0.3141592653589793D1 * (t1125 * t1934 - t1946 - t1126 * t
     #1932 / 0.2D1) - 0.180D3 * t9 * t41 * (t1125 * t1932 - t1934) - t9 
     #* t52 * t1932) * t118 * t58 / 0.720D3
      t1965 = FJET(XB1, XB2, s, -t1100, 0.0D0, -t1098, -t5, t474, t1964)
      t1967 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x
     #4)
      t1968 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x
     #4)
      t1982 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x
     #4)
      t1999 = (0.90D2 * t9 * 0.3141592653589793D1 * (t1967 - t1106 * t19
     #68) - 0.180D3 * t9 * t41 * t1968) * t56 * t119 / 0.720D3 - (0.90D2
     # * t9 * 0.3141592653589793D1 * (-t1126 * t1968 / 0.2D1 - t1982 + t
     #1125 * t1967) - 0.180D3 * t9 * t41 * (t1125 * t1968 - t1967) - t9 
     #* t52 * t1968) * t118 * t58 / 0.720D3
      t2000 = FJET(XB1, XB2, s, -t1098, -t5, -t1100, 0.0D0, t474, t1999)
      t2002 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t479, x4)
      t2004 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t479, x4)
      t2016 = -0.90D2 * t9 * 0.3141592653589793D1 * (t478 * t2002 - t494
     # * t477 * t2004) * t530 + 0.180D3 * t534 * t478 * t2004 * t530
      t2020 = FJET(XB1, XB2, s, -t464, t469, -t432, -t460, t474, t2016 *
     # t56 * t119 / 0.720D3)
      t2024 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t479, x4)
      t2026 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t479, x4)
      t2038 = -0.90D2 * t9 * 0.3141592653589793D1 * (t478 * t2024 - t494
     # * t477 * t2026) * t530 + 0.180D3 * t534 * t478 * t2026 * t530
      t2042 = FJET(XB1, XB2, s, t469, -t464, -t460, -t432, t474, t2038 *
     # t56 * t119 / 0.720D3)
      t2046 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.1
     #0D1, x4)
      t2047 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.1
     #0D1, x4)
      t2052 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.1
     #0D1, x4)
      t2056 = rrqg2qgh81J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.1
     #0D1, x4)
      t2064 = t9 * t88 * t2047
      t2101 = rrqg2qgh81J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.1
     #0D1, x4)
      t2153 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1
     #, x4)
      t2154 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1
     #, x4)
      t2160 = -t2154 + t2047
      t2168 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1
     #, x4)
      t2187 = -t9 * t52 * t2160
      t2223 = rrqg2qgh81J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1
     #, x4)
      t2247 = (t9 * t52 * (t2046 - t153 * t2047) + 0.90D2 * t9 * 0.31415
     #92653589793D1 * (-t153 * t2052 - t164 * t2047 / 0.6D1 + t2056 + t1
     #61 * t2046 / 0.2D1) + t2064 - 0.180D3 * t9 * t41 * (-t153 * t2046 
     #+ t161 * t2047 / 0.2D1 + t2052)) * t56 / 0.1440D4 + (-0.180D3 * (-
     #t187 * t2052 + t188 * t2046 / 0.2D1 - t191 * t2047 / 0.6D1 + t2056
     #) * t6 * t197 + (-t187 * t2046 + t188 * t2047 / 0.2D1 + t2052) * t
     #6 * t205 + (t2046 - t187 * t2047) * t6 * t210 + t2047 * t6 * t220 
     #+ 0.90D2 * (-t187 * t2056 - t191 * t2046 / 0.6D1 + t228 * t2047 / 
     #0.24D2 + t2101 + t188 * t2052 / 0.2D1) * t6 * t8) * 0.314159265358
     #9793D1 / 0.1440D4 - (0.90D2 * t9 * 0.3141592653589793D1 * (-t242 *
     # t2047 / 0.2D1 - t2052 + t240 * t2046) - 0.180D3 * t9 * t41 * (t24
     #0 * t2047 - t2046) - t9 * t52 * t2047) * t56 * t58 / 0.720D3 + (t9
     # * t52 * (t2046 - t263 * t2047) + 0.90D2 * t9 * 0.3141592653589793
     #D1 * (t269 * t2046 / 0.2D1 - t263 * t2052 - t272 * t2047 / 0.6D1 +
     # t2056) + t2064 - 0.180D3 * t9 * t41 * (t269 * t2047 / 0.2D1 - t26
     #3 * t2046 + t2052)) * t58 / 0.720D3 + (0.90D2 * t9 * 0.31415926535
     #89793D1 * (-t302 * t2047 - t2153 + t297 * t2154 + t2046) - 0.180D3
     # * t9 * t41 * t2160) * t56 * t119 / 0.720D3 - (0.90D2 * t9 * 0.314
     #1592653589793D1 * (t2168 + t326 * t2046 + t321 * t2154 / 0.2D1 - t
     #329 * t2047 / 0.2D1 - t320 * t2153 - t2052) - 0.180D3 * t9 * t41 *
     # (-t2046 - t320 * t2154 + t2153 + t326 * t2047) + t2187) * t118 * 
     #t58 / 0.720D3 - (0.90D2 * t9 * 0.3141592653589793D1 * (-t2052 - t3
     #53 * t2153 - t362 * t2047 / 0.2D1 + t357 * t2046 + t359 * t2154 / 
     #0.2D1 + t2168) - 0.180D3 * t9 * t41 * (-t353 * t2154 + t357 * t204
     #7 - t2046 + t2153) + t2187) * t56 * t118 / 0.1440D4 + (t9 * t52 * 
     #(-t2153 + t382 * t2154 + t2046 - t386 * t2047) + 0.90D2 * t9 * 0.3
     #141592653589793D1 * (t400 * t2154 / 0.6D1 - t386 * t2052 + t382 * 
     #t2168 + t2056 - t392 * t2047 / 0.6D1 - t2223 + t391 * t2046 / 0.2D
     #1 - t395 * t2153 / 0.2D1) + t9 * t88 * t2160 - 0.180D3 * t9 * t41 
     #* (-t2168 + t391 * t2047 / 0.2D1 + t2052 + t382 * t2153 - t395 * t
     #2154 / 0.2D1 - t386 * t2046)) * t118 / 0.1440D4
      t2248 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t2247)
      t2250 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t1156,
     # x4)
      t2253 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t1156,
     # x4)
      t2254 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t1156,
     # x4)
      t2282 = -(0.90D2 * t9 * 0.3141592653589793D1 * (-t1415 * t2250 / 0
     #.2D1 - t2253 + t1414 * t2254) - 0.180D3 * t9 * t41 * (-t2254 + t14
     #14 * t2250) - t9 * t52 * t2250) * t56 * t58 / 0.720D3 + (0.90D2 * 
     #t9 * 0.3141592653589793D1 * (t2254 - t1439 * t2250) - 0.180D3 * t9
     # * t41 * t2250) * t56 * t119 / 0.720D3
      t2283 = FJET(XB1, XB2, s, -t1405, t1403, t1407, -t1406, 0.0D0, t22
     #82)
      t2285 = t1528 * t1527 + t1563 * t1562 + t1641 * t1640 + t1716 * t1
     #715 + t1736 * t1732 * t546 / 0.720D3 + t1816 * t1815 + t1855 * t18
     #54 + t1930 * t1929 + t1965 * t1964 + t2000 * t1999 + t2020 * t2016
     # * t546 / 0.720D3 + t2042 * t2038 * t546 / 0.720D3 + t2248 * t2247
     # + t2283 * t2282
      rrqg2qght8s4e1 = t1494 + t2285

      end function



      doubleprecision function rrqg2qght8s4e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x1
      t4 = -0.1D1 + x2
      t5 = x2 * x3
      t6 = x3 * z
      t7 = x3 * x1
      t8 = x1 * z
      t9 = t7 * z
      t10 = x4 * 0.3141592653589793D1
      t11 = cos(t10)
      t12 = -z - x1 + t8
      t14 = -0.1D1 + x3
      t15 = x2 * t14
      t17 = Sqrt(x3 * t12 * t15)
      t18 = t11 * t17
      t19 = 0.2D1 * t18
      t22 = 0.1D1 / t12
      t23 = -0.1D1 + t5
      t24 = 0.1D1 / t23
      t27 = t3 * t4 * (-t5 - z + t6 - x1 + t7 + t8 - t9 + t19) * t22 * t
     #24
      t28 = t14 * s
      t29 = -0.1D1 + x1
      t30 = t1 * t29
      t32 = t28 * t30 * t24
      t33 = t5 * z
      t35 = t5 * x1
      t37 = x2 ** 2
      t38 = t37 * x3
      t39 = t38 * x1
      t43 = t38 * t8
      t46 = t9 + 0.2D1 * t33 + 0.2D1 * t35 - t39 - t38 * z + t5 - 0.2D1 
     #* t5 * t8 + t43 - x2 - t6 - t7 - t19 + 0.2D1 * t18 * x2
      t49 = t3 * t46 * t22 * t24
      t50 = t2 * t29
      t52 = x3 * t4 * t24
      t53 = t50 * t52
      t54 = t1 ** 2
      t59 = s * t54 * x2 * x1 * t29 * t22
      t60 = 0.1D1 / t1
      t61 = s ** 2
      t62 = 0.1D1 / t61
      t63 = t60 * t62
      t65 = x2 * x1
      t66 = t65 * z
      t67 = z + x1 + t66 - t65 - t8
      t69 = t63 * 0.3141592653589793D1 * t12 * t67
      t70 = t14 * t24
      t71 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t70, x4)
      t80 = x1 ** 2
      t81 = x3 * t80
      t83 = z ** 2
      t91 = -0.2D1 * t18 * t65 - 0.2D1 * t18 * t8 + 0.2D1 * t18 * z + 0.
     #2D1 * t18 * x1 + t66 - t33 - t35 - t81 * x2 + t39 + x3 * t83 * t65
     # + 0.2D1 * t81 * x2 * z - t81 * t83 * x2
      t100 = x2 * t80
      t105 = -t43 - t83 - 0.2D1 * t8 + 0.2D1 * t18 * t66 + 0.2D1 * x1 * 
     #t83 - t80 + 0.2D1 * t80 * z - t80 * t83 + t100 - t65 * t83 - 0.2D1
     # * t100 * z + t100 * t83
      t107 = 0.1D1 / (t91 + t105)
      t109 = 0.1D1 / x3
      t110 = 0.1D1 / x2
      t112 = 0.1D1 / x1
      t113 = t109 * t110 * t112
      t117 = FJET(XB1, XB2, s, t27, -t32, -t49, -t53, t59, -t69 * t71 * 
     #t107 * t113 / 0.8D1)
      t120 = t62 * 0.3141592653589793D1 * t12
      t128 = t2 * t65 * t22
      t130 = t1 * x1
      t131 = t4 * s * t130
      t132 = t63 * 0.3141592653589793D1
      t133 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t135 = t110 * t112
      t139 = t54 ** 2
      t140 = Sin(t10)
      t141 = t140 ** 2
      t142 = t139 * t141
      t144 = 0.1D1 / t83
      t145 = t144 * t22
      t146 = t29 ** 2
      t147 = t4 ** 2
      t152 = log(-0.4D1 * t100 * t142 * t145 * t146 * t147)
      t154 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t159 = 0.3141592653589793D1 * lh
      t167 = t132 * t133 * t109 * t135 / 0.8D1 - (0.90D2 * t63 * 0.31415
     #92653589793D1 * (t152 * t133 - t154) + 0.180D3 * t63 * t159 * t133
     #) * t110 * t112 / 0.720D3
      t168 = FJET(XB1, XB2, s, -t128, 0.0D0, -t131, -t50, t59, t167)
      t170 = t2 * x3
      t171 = t2 * t14
      t173 = 0.1D1 / t83 / z
      t174 = x3 * t173
      t175 = t142 * t14
      t178 = log(-0.4D1 * t174 * t175)
      t179 = t178 ** 2
      t180 = -t14
      t181 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t180
     #, x4)
      t184 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t180
     #, x4)
      t185 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t180
     #, x4)
      t196 = lh ** 2
      t198 = 0.3141592653589793D1 ** 2
      t200 = 0.180D3 * t196 - 0.30D2 * t198
      t201 = t200 * 0.3141592653589793D1
      t207 = t5 * t173
      t210 = log(-0.4D1 * t207 * t175)
      t218 = 0.180D3 * t63 * t159 * t181
      t224 = t141 * t173
      t228 = log(-0.4D1 * t81 * t139 * t224 * t14)
      t242 = (0.90D2 * t63 * 0.3141592653589793D1 * (-t179 * t181 / 0.2D
     #1 - t184 + t178 * t185) - 0.180D3 * t63 * t159 * (-t185 + t178 * t
     #181) - t63 * t201 * t181) * t109 / 0.1440D4 - (0.90D2 * t63 * 0.31
     #41592653589793D1 * (t185 - t210 * t181) - t218) * t109 * t110 / 0.
     #1440D4 - (0.90D2 * t63 * 0.3141592653589793D1 * (-t228 * t181 + t1
     #85) - t218) * t109 * t112 / 0.720D3 - t132 * t181 * t109 * t135 / 
     #0.8D1
      t243 = FJET(XB1, XB2, s, t170, 0.0D0, -t171, 0.0D0, 0.0D0, t242)
      t245 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t180
     #, x4)
      t247 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t180
     #, x4)
      t254 = 0.180D3 * t63 * t159 * t245
      t266 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t180
     #, x4)
      t290 = -(0.90D2 * t63 * 0.3141592653589793D1 * (-t228 * t245 + t24
     #7) - t254) * t109 * t112 / 0.720D3 - t132 * t245 * t109 * t135 / 0
     #.8D1 + (0.90D2 * t63 * 0.3141592653589793D1 * (t178 * t247 - t179 
     #* t245 / 0.2D1 - t266) - 0.180D3 * t63 * t159 * (-t247 + t178 * t2
     #45) - t63 * t201 * t245) * t109 / 0.1440D4 - (0.90D2 * t63 * 0.314
     #1592653589793D1 * (-t210 * t245 + t247) - t254) * t109 * t110 / 0.
     #1440D4
      t291 = FJET(XB1, XB2, s, 0.0D0, -t171, 0.0D0, t170, 0.0D0, t290)
      t295 = t22 * t146
      t299 = log(-0.4D1 * t81 * t141 * t144 * t139 * t295)
      t300 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t302 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t309 = 0.180D3 * t63 * t159 * t300
      t318 = t100 * t139
      t323 = log(-0.4D1 * t318 * t141 * t144 * t295)
      t333 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t334 = t80 * t141
      t340 = log(-0.4D1 * t334 * t144 * t139 * t22 * t146)
      t341 = t340 ** 2
      t359 = -(0.90D2 * t63 * 0.3141592653589793D1 * (-t299 * t300 + t30
     #2) - t309) * t109 * t112 / 0.720D3 - t132 * t300 * t109 * t135 / 0
     #.8D1 - (0.90D2 * t63 * 0.3141592653589793D1 * (-t323 * t300 + t302
     #) - t309) * t110 * t112 / 0.720D3 + (0.90D2 * t63 * 0.314159265358
     #9793D1 * (-t333 - t341 * t300 / 0.2D1 + t340 * t302) - 0.180D3 * t
     #63 * t159 * (t340 * t300 - t302) - t63 * t201 * t300) * t112 / 0.7
     #20D3
      t360 = FJET(XB1, XB2, s, -t50, t3, 0.0D0, 0.0D0, 0.0D0, t359)
      t362 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t368 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t380 = t132 * t362 * t109 * t135 / 0.8D1 - (0.90D2 * t63 * 0.31415
     #92653589793D1 * (t152 * t362 - t368) + 0.180D3 * t63 * t159 * t362
     #) * t110 * t112 / 0.720D3
      t381 = FJET(XB1, XB2, s, -t131, -t50, -t128, 0.0D0, t59, t380)
      t383 = t2 * t52
      t384 = t2 * t70
      t385 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t70, x4
     #)
      t386 = 0.3141592653589793D1 * t385
      t389 = Sqrt(-t6 * t15)
      t393 = 0.1D1 / (-z - t5 + 0.2D1 * t11 * t389)
      t394 = z * t393
      t395 = t394 * t113
      t398 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t70, x4
     #)
      t399 = t173 * t139
      t402 = t23 ** 2
      t408 = log(-0.4D1 * t5 * t399 * t141 * t147 * t14 / t402)
      t415 = t63 * lh
      t423 = -t63 * t386 * t395 / 0.8D1 - (0.90D2 * t132 * (t398 - t408 
     #* t385) * z * t393 - 0.180D3 * t415 * t386 * t394) * t109 * t110 /
     # 0.1440D4
      t424 = FJET(XB1, XB2, s, 0.0D0, t383, 0.0D0, t384, 0.0D0, t423)
      t427 = x3 * s * t30
      t428 = t2 * t7
      t429 = t28 * t30
      t430 = t28 * t130
      t431 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t180, x
     #4)
      t437 = log(0.4D1 * t81 * t142 * t145 * t146 * t14)
      t438 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t180, x
     #4)
      t455 = -(0.90D2 * t63 * 0.3141592653589793D1 * (-t431 + t437 * t43
     #8) + 0.180D3 * t63 * t159 * t438) * t109 * t112 / 0.720D3 + t132 *
     # t438 * t109 * t135 / 0.8D1
      t456 = FJET(XB1, XB2, s, -t427, t428, t429, -t430, 0.0D0, t455)
      t458 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t180
     #, x4)
      t459 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t180
     #, x4)
      t467 = 0.180D3 * t63 * t159 * t459
      t475 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t180
     #, x4)
      t503 = -(0.90D2 * t63 * 0.3141592653589793D1 * (t458 - t210 * t459
     #) - t467) * t109 * t110 / 0.1440D4 + (0.90D2 * t63 * 0.31415926535
     #89793D1 * (-t179 * t459 / 0.2D1 + t178 * t458 - t475) - 0.180D3 * 
     #t63 * t159 * (t178 * t459 - t458) - t63 * t201 * t459) * t109 / 0.
     #1440D4 - (0.90D2 * t63 * 0.3141592653589793D1 * (t458 - t228 * t45
     #9) - t467) * t109 * t112 / 0.720D3 - t132 * t459 * t109 * t135 / 0
     #.8D1
      t504 = FJET(XB1, XB2, s, 0.0D0, t170, 0.0D0, -t171, 0.0D0, t503)
      t506 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t180, x
     #4)
      t508 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t180, x
     #4)
      t524 = -(0.90D2 * t63 * 0.3141592653589793D1 * (t437 * t506 - t508
     #) + 0.180D3 * t63 * t159 * t506) * t109 * t112 / 0.720D3 + t132 * 
     #t506 * t109 * t135 / 0.8D1
      t525 = FJET(XB1, XB2, s, t429, -t430, -t427, t428, 0.0D0, t524)
      t527 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t180, x
     #4)
      t528 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t180, x
     #4)
      t545 = -(0.90D2 * t63 * 0.3141592653589793D1 * (-t527 + t437 * t52
     #8) + 0.180D3 * t63 * t159 * t528) * t109 * t112 / 0.720D3 + t132 *
     # t528 * t109 * t135 / 0.8D1
      t546 = FJET(XB1, XB2, s, -t430, t429, t428, -t427, 0.0D0, t545)
      t548 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t554 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t566 = t132 * t548 * t109 * t135 / 0.8D1 - (0.90D2 * t63 * 0.31415
     #92653589793D1 * (t152 * t548 - t554) + 0.180D3 * t63 * t159 * t548
     #) * t110 * t112 / 0.720D3
      t567 = FJET(XB1, XB2, s, -t50, -t131, 0.0D0, -t128, t59, t566)
      t569 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t570 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t578 = 0.180D3 * t63 * t159 * t570
      t596 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t614 = -(0.90D2 * t63 * 0.3141592653589793D1 * (t569 - t299 * t570
     #) - t578) * t109 * t112 / 0.720D3 - t132 * t570 * t109 * t135 / 0.
     #8D1 - (0.90D2 * t63 * 0.3141592653589793D1 * (t569 - t323 * t570) 
     #- t578) * t110 * t112 / 0.720D3 + (0.90D2 * t63 * 0.31415926535897
     #93D1 * (-t596 + t340 * t569 - t341 * t570 / 0.2D1) - 0.180D3 * t63
     # * t159 * (t340 * t570 - t569) - t63 * t201 * t570) * t112 / 0.720
     #D3
      t615 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t50, 0.0D0, t614)
      t617 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t70, x4
     #)
      t618 = 0.3141592653589793D1 * t617
      t622 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t70, x4
     #)
      t636 = -t63 * t618 * t395 / 0.8D1 - (0.90D2 * t132 * (t622 - t408 
     #* t617) * z * t393 - 0.180D3 * t415 * t618 * t394) * t109 * t110 /
     # 0.1440D4
      t637 = FJET(XB1, XB2, s, 0.0D0, t384, 0.0D0, t383, 0.0D0, t636)
      t639 = -t117 * t60 * t120 * t67 * t71 * t107 * t113 / 0.8D1 + t168
     # * t167 + t243 * t242 + t291 * t290 + t360 * t359 + t381 * t380 + 
     #t424 * t423 + t456 * t455 + t504 * t503 + t525 * t524 + t546 * t54
     #5 + t567 * t566 + t615 * t614 + t637 * t636
      t642 = log(0.4D1 * t174 * t142)
      t643 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t645 = t642 ** 2
      t646 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t649 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t660 = t63 * t201 * t646
      t664 = t224 * t139
      t666 = log(0.4D1 * t664)
      t668 = t666 ** 2
      t673 = t62 * lh
      t682 = t62 * (0.60D2 * lh * t198 - 0.2884936567583026D3 - 0.120D3 
     #* t196 * lh)
      t687 = t668 * t666
      t690 = rrqg2qgh81J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t698 = t62 * t200
      t703 = t142 * t147
      t706 = log(0.4D1 * t207 * t703)
      t707 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t711 = log(0.4D1 * t5 * t664)
      t713 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t718 = -t646 + t707
      t721 = 0.180D3 * t63 * t159 * t718
      t726 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t727 = x2 * t173
      t730 = log(0.4D1 * t727 * t142)
      t731 = t730 ** 2
      t736 = log(0.4D1 * t727 * t703)
      t738 = t736 ** 2
      t752 = -t718
      t760 = log(0.4D1 * t81 * t664)
      t780 = log(0.4D1 * t318 * t224 * t147)
      t784 = log(0.4D1 * t100 * t664)
      t796 = log(0.4D1 * t334 * t399)
      t797 = t796 ** 2
      t813 = (0.90D2 * t63 * 0.3141592653589793D1 * (-t642 * t643 + t645
     # * t646 / 0.2D1 + t649) - 0.180D3 * t63 * t159 * (t643 - t642 * t6
     #46) + t660) * t109 / 0.1440D4 + (-0.180D3 * (-t666 * t643 + t668 *
     # t646 / 0.2D1 + t649) * t60 * t673 + t646 * t60 * t682 + 0.90D2 * 
     #(-t666 * t649 + t668 * t643 / 0.2D1 - t687 * t646 / 0.6D1 + t690) 
     #* t60 * t62 + (t643 - t666 * t646) * t60 * t698) * 0.3141592653589
     #793D1 / 0.1440D4 - (0.90D2 * t63 * 0.3141592653589793D1 * (-t706 *
     # t707 + t711 * t646 - t643 + t713) - t721) * t109 * t110 / 0.1440D
     #4 + (0.90D2 * t63 * 0.3141592653589793D1 * (-t726 + t731 * t646 / 
     #0.2D1 + t649 + t736 * t713 - t738 * t707 / 0.2D1 - t730 * t643) - 
     #0.180D3 * t63 * t159 * (-t713 + t736 * t707 + t643 - t730 * t646) 
     #+ t63 * t201 * t752) * t110 / 0.1440D4 - (0.90D2 * t63 * 0.3141592
     #653589793D1 * (t760 * t646 - t643) + 0.180D3 * t63 * t159 * t646) 
     #* t109 * t112 / 0.720D3 + t132 * t752 * t109 * t135 / 0.8D1 - (0.9
     #0D2 * t63 * 0.3141592653589793D1 * (-t643 - t780 * t707 + t713 + t
     #784 * t646) - t721) * t110 * t112 / 0.720D3 + (0.90D2 * t63 * 0.31
     #41592653589793D1 * (t797 * t646 / 0.2D1 - t796 * t643 + t649) - 0.
     #180D3 * t63 * t159 * (t643 - t796 * t646) + t660) * t112 / 0.720D3
      t814 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t813)
      t816 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t817 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t819 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t832 = t63 * t201 * t819
      t849 = rrqg2qgh82J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t862 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t864 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t870 = t862 - t819
      t873 = 0.180D3 * t63 * t159 * t870
      t882 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t895 = -t870
      t942 = (0.90D2 * t63 * 0.3141592653589793D1 * (t816 - t642 * t817 
     #+ t645 * t819 / 0.2D1) - 0.180D3 * t63 * t159 * (t817 - t642 * t81
     #9) + t832) * t109 / 0.1440D4 + (-0.180D3 * (t816 - t666 * t817 + t
     #668 * t819 / 0.2D1) * t60 * t673 + t819 * t60 * t682 + 0.90D2 * (t
     #668 * t817 / 0.2D1 - t687 * t819 / 0.6D1 + t849 - t666 * t816) * t
     #60 * t62 + (t817 - t666 * t819) * t60 * t698) * 0.3141592653589793
     #D1 / 0.1440D4 - (0.90D2 * t63 * 0.3141592653589793D1 * (-t706 * t8
     #62 - t817 + t864 + t711 * t819) - t873) * t109 * t110 / 0.1440D4 +
     # (0.90D2 * t63 * 0.3141592653589793D1 * (t736 * t864 - t738 * t862
     # / 0.2D1 + t816 - t730 * t817 - t882 + t731 * t819 / 0.2D1) - 0.18
     #0D3 * t63 * t159 * (t736 * t862 - t864 + t817 - t730 * t819) + t63
     # * t201 * t895) * t110 / 0.1440D4 - (0.90D2 * t63 * 0.314159265358
     #9793D1 * (t760 * t819 - t817) + 0.180D3 * t63 * t159 * t819) * t10
     #9 * t112 / 0.720D3 + t132 * t895 * t109 * t135 / 0.8D1 - (0.90D2 *
     # t63 * 0.3141592653589793D1 * (-t780 * t862 - t817 + t864 + t784 *
     # t819) - t873) * t110 * t112 / 0.720D3 + (0.90D2 * t63 * 0.3141592
     #653589793D1 * (t816 - t796 * t817 + t797 * t819 / 0.2D1) - 0.180D3
     # * t63 * t159 * (t817 - t796 * t819) + t832) * t112 / 0.720D3
      t943 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t942)
      t945 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t70, x4)
      t950 = FJET(XB1, XB2, s, -t49, -t53, t27, -t32, t59, -t69 * t945 *
     # t107 * t113 / 0.8D1)
      t958 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t959 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t961 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t974 = t63 * t201 * t961
      t990 = rrqg2qgh84J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t1004 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1
     #, x4)
      t1007 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1
     #, x4)
      t1012 = t1004 - t961
      t1015 = 0.180D3 * t63 * t159 * t1012
      t1022 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1
     #, x4)
      t1037 = -t1012
      t1084 = (0.90D2 * t63 * 0.3141592653589793D1 * (t958 - t642 * t959
     # + t645 * t961 / 0.2D1) - 0.180D3 * t63 * t159 * (t959 - t642 * t9
     #61) + t974) * t109 / 0.1440D4 + (-0.180D3 * (t958 - t666 * t959 + 
     #t668 * t961 / 0.2D1) * t60 * t673 + t961 * t60 * t682 + 0.90D2 * (
     #t668 * t959 / 0.2D1 - t666 * t958 + t990 - t687 * t961 / 0.6D1) * 
     #t60 * t62 + (t959 - t666 * t961) * t60 * t698) * 0.314159265358979
     #3D1 / 0.1440D4 - (0.90D2 * t63 * 0.3141592653589793D1 * (-t706 * t
     #1004 - t959 + t711 * t961 + t1007) - t1015) * t109 * t110 / 0.1440
     #D4 + (0.90D2 * t63 * 0.3141592653589793D1 * (t731 * t961 / 0.2D1 -
     # t1022 - t738 * t1004 / 0.2D1 + t736 * t1007 - t730 * t959 + t958)
     # - 0.180D3 * t63 * t159 * (-t1007 + t736 * t1004 + t959 - t730 * t
     #961) + t63 * t201 * t1037) * t110 / 0.1440D4 - (0.90D2 * t63 * 0.3
     #141592653589793D1 * (-t959 + t760 * t961) + 0.180D3 * t63 * t159 *
     # t961) * t109 * t112 / 0.720D3 + t132 * t1037 * t109 * t135 / 0.8D
     #1 - (0.90D2 * t63 * 0.3141592653589793D1 * (t1007 - t959 - t780 * 
     #t1004 + t784 * t961) - t1015) * t110 * t112 / 0.720D3 + (0.90D2 * 
     #t63 * 0.3141592653589793D1 * (t958 - t796 * t959 + t797 * t961 / 0
     #.2D1) - 0.180D3 * t63 * t159 * (t959 - t796 * t961) + t974) * t112
     # / 0.720D3
      t1085 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t1084)
      t1087 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1
     #, x4)
      t1088 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1
     #, x4)
      t1096 = 0.180D3 * t63 * t159 * t1088
      t1114 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1
     #, x4)
      t1132 = -(0.90D2 * t63 * 0.3141592653589793D1 * (t1087 - t299 * t1
     #088) - t1096) * t109 * t112 / 0.720D3 - t132 * t1088 * t109 * t135
     # / 0.8D1 - (0.90D2 * t63 * 0.3141592653589793D1 * (-t323 * t1088 +
     # t1087) - t1096) * t110 * t112 / 0.720D3 + (0.90D2 * t63 * 0.31415
     #92653589793D1 * (-t1114 - t341 * t1088 / 0.2D1 + t340 * t1087) - 0
     #.180D3 * t63 * t159 * (t340 * t1088 - t1087) - t63 * t201 * t1088)
     # * t112 / 0.720D3
      t1133 = FJET(XB1, XB2, s, t3, -t50, 0.0D0, 0.0D0, 0.0D0, t1132)
      t1135 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t70, x4)
      t1140 = FJET(XB1, XB2, s, -t32, t27, -t53, -t49, t59, -t69 * t1135
     # * t107 * t113 / 0.8D1)
      t1148 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t70, x
     #4)
      t1149 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t70, x
     #4)
      t1156 = 0.3141592653589793D1 * t1149
      t1167 = -(0.90D2 * t132 * (t1148 - t408 * t1149) * z * t393 - 0.18
     #0D3 * t415 * t1156 * t394) * t109 * t110 / 0.1440D4 - t63 * t1156 
     #* t395 / 0.8D1
      t1168 = FJET(XB1, XB2, s, t384, 0.0D0, t383, 0.0D0, 0.0D0, t1167)
      t1170 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1
     #, x4)
      t1171 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1
     #, x4)
      t1179 = 0.180D3 * t63 * t159 * t1171
      t1200 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1
     #, x4)
      t1215 = -(0.90D2 * t63 * 0.3141592653589793D1 * (t1170 - t299 * t1
     #171) - t1179) * t109 * t112 / 0.720D3 - t132 * t1171 * t109 * t135
     # / 0.8D1 - (0.90D2 * t63 * 0.3141592653589793D1 * (-t323 * t1171 +
     # t1170) - t1179) * t110 * t112 / 0.720D3 + (0.90D2 * t63 * 0.31415
     #92653589793D1 * (-t341 * t1171 / 0.2D1 + t340 * t1170 - t1200) - 0
     #.180D3 * t63 * t159 * (t340 * t1171 - t1170) - t63 * t201 * t1171)
     # * t112 / 0.720D3
      t1216 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t50, t3, 0.0D0, t1215)
      t1218 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t180, 
     #x4)
      t1220 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t180, 
     #x4)
      t1236 = -(0.90D2 * t63 * 0.3141592653589793D1 * (t437 * t1218 - t1
     #220) + 0.180D3 * t63 * t159 * t1218) * t109 * t112 / 0.720D3 + t13
     #2 * t1218 * t109 * t135 / 0.8D1
      t1237 = FJET(XB1, XB2, s, t428, -t427, -t430, t429, 0.0D0, t1236)
      t1239 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.1
     #0D1, x4)
      t1242 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.1
     #0D1, x4)
      t1243 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.1
     #0D1, x4)
      t1255 = t63 * t201 * t1239
      t1259 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1
     #, x4)
      t1261 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1
     #, x4)
      t1267 = -t1239 + t1261
      t1270 = 0.180D3 * t63 * t159 * t1267
      t1275 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1
     #, x4)
      t1292 = -t1267
      t1353 = rrqg2qgh83J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.1
     #0D1, x4)
      t1365 = (0.90D2 * t63 * 0.3141592653589793D1 * (t645 * t1239 / 0.2
     #D1 + t1242 - t642 * t1243) - 0.180D3 * t63 * t159 * (t1243 - t642 
     #* t1239) + t1255) * t109 / 0.1440D4 - (0.90D2 * t63 * 0.3141592653
     #589793D1 * (t1259 - t1243 + t711 * t1239 - t706 * t1261) - t1270) 
     #* t109 * t110 / 0.1440D4 + (0.90D2 * t63 * 0.3141592653589793D1 * 
     #(-t1275 + t731 * t1239 / 0.2D1 - t730 * t1243 + t736 * t1259 - t73
     #8 * t1261 / 0.2D1 + t1242) - 0.180D3 * t63 * t159 * (-t1259 + t124
     #3 + t736 * t1261 - t730 * t1239) + t63 * t201 * t1292) * t110 / 0.
     #1440D4 - (0.90D2 * t63 * 0.3141592653589793D1 * (-t1243 + t760 * t
     #1239) + 0.180D3 * t63 * t159 * t1239) * t109 * t112 / 0.720D3 + t1
     #32 * t1292 * t109 * t135 / 0.8D1 - (0.90D2 * t63 * 0.3141592653589
     #793D1 * (t1259 - t780 * t1261 + t784 * t1239 - t1243) - t1270) * t
     #110 * t112 / 0.720D3 + (0.90D2 * t63 * 0.3141592653589793D1 * (t12
     #42 - t796 * t1243 + t797 * t1239 / 0.2D1) - 0.180D3 * t63 * t159 *
     # (-t796 * t1239 + t1243) + t1255) * t112 / 0.720D3 + (-0.180D3 * (
     #-t666 * t1243 + t668 * t1239 / 0.2D1 + t1242) * t60 * t673 + t1239
     # * t60 * t682 + 0.90D2 * (t668 * t1243 / 0.2D1 - t687 * t1239 / 0.
     #6D1 - t666 * t1242 + t1353) * t60 * t62 + (t1243 - t666 * t1239) *
     # t60 * t698) * 0.3141592653589793D1 / 0.1440D4
      t1366 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t1365)
      t1368 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x
     #4)
      t1373 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x
     #4)
      t1386 = t132 * t1368 * t109 * t135 / 0.8D1 - (0.90D2 * t63 * 0.314
     #1592653589793D1 * (-t1373 + t152 * t1368) + 0.180D3 * t63 * t159 *
     # t1368) * t110 * t112 / 0.720D3
      t1387 = FJET(XB1, XB2, s, 0.0D0, -t128, -t50, -t131, t59, t1386)
      t1389 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t70, x
     #4)
      t1390 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t70, x
     #4)
      t1397 = 0.3141592653589793D1 * t1390
      t1408 = -(0.90D2 * t132 * (t1389 - t408 * t1390) * z * t393 - 0.18
     #0D3 * t415 * t1397 * t394) * t109 * t110 / 0.1440D4 - t63 * t1397 
     #* t395 / 0.8D1
      t1409 = FJET(XB1, XB2, s, t383, 0.0D0, t384, 0.0D0, 0.0D0, t1408)
      t1411 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t18
     #0, x4)
      t1413 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t18
     #0, x4)
      t1414 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t18
     #0, x4)
      t1438 = 0.180D3 * t63 * t159 * t1414
      t1456 = (0.90D2 * t63 * 0.3141592653589793D1 * (t178 * t1411 - t14
     #13 - t179 * t1414 / 0.2D1) - 0.180D3 * t63 * t159 * (-t1411 + t178
     # * t1414) - t63 * t201 * t1414) * t109 / 0.1440D4 - (0.90D2 * t63 
     #* 0.3141592653589793D1 * (t1411 - t210 * t1414) - t1438) * t109 * 
     #t110 / 0.1440D4 - (0.90D2 * t63 * 0.3141592653589793D1 * (t1411 - 
     #t228 * t1414) - t1438) * t109 * t112 / 0.720D3 - t132 * t1414 * t1
     #09 * t135 / 0.8D1
      t1457 = FJET(XB1, XB2, s, -t171, 0.0D0, t170, 0.0D0, 0.0D0, t1456)
      t1459 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t70, x4)
      t1464 = FJET(XB1, XB2, s, -t53, -t49, -t32, t27, t59, -t69 * t1459
     # * t107 * t113 / 0.8D1)
      t1472 = t814 * t813 + t943 * t942 - t950 * t60 * t120 * t67 * t945
     # * t107 * t113 / 0.8D1 + t1085 * t1084 + t1133 * t1132 - t1140 * t
     #60 * t120 * t67 * t1135 * t107 * t113 / 0.8D1 + t1168 * t1167 + t1
     #216 * t1215 + t1237 * t1236 + t1366 * t1365 + t1387 * t1386 + t140
     #9 * t1408 + t1457 * t1456 - t1464 * t60 * t120 * t67 * t1459 * t10
     #7 * t113 / 0.8D1
      rrqg2qght8s4e0 = t639 + t1472

      end function



      doubleprecision function rrqg2qght8s4em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10D1
     #, x4)
      t8 = z ** 2
      t10 = 0.1D1 / t8 / z
      t11 = x3 * t10
      t12 = x4 * 0.3141592653589793D1
      t13 = Sin(t12)
      t14 = t13 ** 2
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t14 * t16
      t20 = log(0.4D1 * t11 * t17)
      t21 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10D
     #1, x4)
      t27 = 0.3141592653589793D1 * lh
      t30 = 0.180D3 * t6 * t27 * t21
      t32 = 0.1D1 / x3
      t38 = log(0.4D1 * t10 * t14 * t16)
      t42 = t5 * lh
      t45 = rrqg2qgh82J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10D
     #1, x4)
      t47 = t38 ** 2
      t55 = lh ** 2
      t57 = 0.3141592653589793D1 ** 2
      t60 = t5 * (0.180D3 * t55 - 0.30D2 * t57)
      t65 = t6 * 0.3141592653589793D1
      t66 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #x4)
      t67 = t66 - t21
      t69 = 0.1D1 / x2
      t73 = x2 * t10
      t74 = -0.1D1 + x2
      t75 = t74 ** 2
      t79 = log(0.4D1 * t73 * t17 * t75)
      t81 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #x4)
      t84 = log(0.4D1 * t73 * t17)
      t98 = 0.1D1 / x1
      t102 = x1 ** 2
      t103 = t102 * t14
      t107 = log(0.4D1 * t103 * t10 * t16)
      t120 = (0.90D2 * t6 * 0.3141592653589793D1 * (t7 - t20 * t21) - t3
     #0) * t32 / 0.1440D4 + (-0.180D3 * (t7 - t38 * t21) * t3 * t42 + 0.
     #90D2 * (t45 - t38 * t7 + t47 * t21 / 0.2D1) * t3 * t5 + t21 * t3 *
     # t60) * 0.3141592653589793D1 / 0.1440D4 - t65 * t67 * t32 * t69 / 
     #0.16D2 + (0.90D2 * t6 * 0.3141592653589793D1 * (t79 * t66 - t81 + 
     #t7 - t84 * t21) + 0.180D3 * t6 * t27 * t67) * t69 / 0.1440D4 - t65
     # * t67 * t69 * t98 / 0.8D1 + (0.90D2 * t6 * 0.3141592653589793D1 *
     # (t7 - t107 * t21) - t30) * t98 / 0.720D3 + t65 * t21 * t32 * t98 
     #/ 0.8D1
      t121 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t120)
      t123 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t124 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t132 = 0.180D3 * t6 * t27 * t124
      t141 = rrqg2qgh84J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t154 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t155 = t154 - t124
      t160 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t190 = (0.90D2 * t6 * 0.3141592653589793D1 * (t123 - t20 * t124) -
     # t132) * t32 / 0.1440D4 + (-0.180D3 * (t123 - t38 * t124) * t3 * t
     #42 + 0.90D2 * (t141 - t38 * t123 + t47 * t124 / 0.2D1) * t3 * t5 +
     # t124 * t3 * t60) * 0.3141592653589793D1 / 0.1440D4 - t65 * t155 *
     # t32 * t69 / 0.16D2 + (0.90D2 * t6 * 0.3141592653589793D1 * (-t160
     # + t79 * t154 + t123 - t84 * t124) + 0.180D3 * t6 * t27 * t155) * 
     #t69 / 0.1440D4 - t65 * t155 * t69 * t98 / 0.8D1 + (0.90D2 * t6 * 0
     #.3141592653589793D1 * (t123 - t107 * t124) - t132) * t98 / 0.720D3
     # + t65 * t124 * t32 * t98 / 0.8D1
      t191 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t190)
      t193 = t2 * x1
      t194 = -0.1D1 + x1
      t195 = t2 * t194
      t196 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t205 = 0.1D1 / (-z - x1 + x1 * z)
      t207 = t194 ** 2
      t211 = log(-0.4D1 * t103 / t8 * t16 * t205 * t207)
      t213 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t228 = -t65 * t196 * t69 * t98 / 0.8D1 + (0.90D2 * t6 * 0.31415926
     #53589793D1 * (t211 * t196 - t213) + 0.180D3 * t6 * t27 * t196) * t
     #98 / 0.720D3 - t65 * t196 * t32 * t98 / 0.8D1
      t229 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t193, -t195, 0.0D0, t228)
      t231 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t237 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t252 = -t65 * t231 * t69 * t98 / 0.8D1 + (0.90D2 * t6 * 0.31415926
     #53589793D1 * (t211 * t231 - t237) + 0.180D3 * t6 * t27 * t231) * t
     #98 / 0.720D3 - t65 * t231 * t32 * t98 / 0.8D1
      t253 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t195, t193, 0.0D0, t252)
      t255 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t256 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t264 = 0.180D3 * t6 * t27 * t256
      t276 = rrqg2qgh81J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t286 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t287 = -t256 + t286
      t292 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t322 = (0.90D2 * t6 * 0.3141592653589793D1 * (t255 - t20 * t256) -
     # t264) * t32 / 0.1440D4 + (-0.180D3 * (t255 - t38 * t256) * t3 * t
     #42 + 0.90D2 * (-t38 * t255 + t47 * t256 / 0.2D1 + t276) * t3 * t5 
     #+ t256 * t3 * t60) * 0.3141592653589793D1 / 0.1440D4 - t65 * t287 
     #* t32 * t69 / 0.16D2 + (0.90D2 * t6 * 0.3141592653589793D1 * (-t29
     #2 + t79 * t286 + t255 - t84 * t256) + 0.180D3 * t6 * t27 * t287) *
     # t69 / 0.1440D4 - t65 * t287 * t69 * t98 / 0.8D1 + (0.90D2 * t6 * 
     #0.3141592653589793D1 * (t255 - t107 * t256) - t264) * t98 / 0.720D
     #3 + t65 * t256 * t32 * t98 / 0.8D1
      t323 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t322)
      t325 = t2 * x3
      t326 = -0.1D1 + x3
      t327 = t2 * t326
      t328 = -t326
      t329 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t328
     #, x4)
      t330 = t329 * t32
      t337 = log(-0.4D1 * t11 * t17 * t326)
      t339 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t328
     #, x4)
      t353 = -t65 * t330 * t69 / 0.16D2 + (0.90D2 * t6 * 0.3141592653589
     #793D1 * (t337 * t329 - t339) + 0.180D3 * t6 * t27 * t329) * t32 / 
     #0.1440D4 - t65 * t330 * t98 / 0.8D1
      t354 = FJET(XB1, XB2, s, 0.0D0, t325, 0.0D0, -t327, 0.0D0, t353)
      t356 = x2 * x3
      t358 = 0.1D1 / (-0.1D1 + t356)
      t359 = t326 * t358
      t360 = t2 * t359
      t363 = t2 * x3 * t74 * t358
      t364 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t359, x
     #4)
      t367 = cos(t12)
      t371 = Sqrt(-x3 * z * x2 * t326)
      t375 = 0.1D1 / (-z - t356 + 0.2D1 * t367 * t371)
      t378 = z * t375 * t32 * t69
      t381 = FJET(XB1, XB2, s, 0.0D0, t360, 0.0D0, t363, 0.0D0, -t6 * 0.
     #3141592653589793D1 * t364 * t378 / 0.16D2)
      t383 = t5 * 0.3141592653589793D1
      t387 = t375 * t32 * t69
      t391 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t359, x
     #4)
      t396 = FJET(XB1, XB2, s, 0.0D0, t363, 0.0D0, t360, 0.0D0, -t6 * 0.
     #3141592653589793D1 * t391 * t378 / 0.16D2)
      t403 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t328
     #, x4)
      t404 = t403 * t32
      t408 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t328
     #, x4)
      t423 = -t65 * t404 * t98 / 0.8D1 + (0.90D2 * t6 * 0.31415926535897
     #93D1 * (-t408 + t337 * t403) + 0.180D3 * t6 * t27 * t403) * t32 / 
     #0.1440D4 - t65 * t404 * t69 / 0.16D2
      t424 = FJET(XB1, XB2, s, 0.0D0, -t327, 0.0D0, t325, 0.0D0, t423)
      t428 = t2 * x1 * x2 * t205
      t430 = t1 * x1
      t431 = t74 * s * t430
      t436 = s * t15 * x2 * x1 * t194 * t205
      t437 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t442 = FJET(XB1, XB2, s, 0.0D0, -t428, -t195, -t431, t436, t65 * t
     #437 * t69 * t98 / 0.8D1)
      t446 = t69 * t98
      t450 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t451 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t459 = 0.180D3 * t6 * t27 * t451
      t463 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t464 = -t451 + t463
      t469 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t507 = rrqg2qgh83J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t517 = (0.90D2 * t6 * 0.3141592653589793D1 * (t450 - t20 * t451) -
     # t459) * t32 / 0.1440D4 - t65 * t464 * t32 * t69 / 0.16D2 + (0.90D
     #2 * t6 * 0.3141592653589793D1 * (-t469 + t450 + t79 * t463 - t84 *
     # t451) + 0.180D3 * t6 * t27 * t464) * t69 / 0.1440D4 - t65 * t464 
     #* t69 * t98 / 0.8D1 + (0.90D2 * t6 * 0.3141592653589793D1 * (-t107
     # * t451 + t450) - t459) * t98 / 0.720D3 + t65 * t451 * t32 * t98 /
     # 0.8D1 + (-0.180D3 * (t450 - t38 * t451) * t3 * t42 + 0.90D2 * (-t
     #38 * t450 + t47 * t451 / 0.2D1 + t507) * t3 * t5 + t451 * t3 * t60
     #) * 0.3141592653589793D1 / 0.1440D4
      t518 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t517)
      t520 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t526 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t541 = -t65 * t520 * t69 * t98 / 0.8D1 + (0.90D2 * t6 * 0.31415926
     #53589793D1 * (t211 * t520 - t526) + 0.180D3 * t6 * t27 * t520) * t
     #98 / 0.720D3 - t65 * t520 * t32 * t98 / 0.8D1
      t542 = FJET(XB1, XB2, s, t193, -t195, 0.0D0, 0.0D0, 0.0D0, t541)
      t544 = t121 * t120 + t191 * t190 + t229 * t228 + t253 * t252 + t32
     #3 * t322 + t354 * t353 - t381 * t3 * t383 * t364 * z * t387 / 0.16
     #D2 - t396 * t3 * t383 * t391 * z * t387 / 0.16D2 + t424 * t423 + t
     #442 * t3 * t5 * 0.3141592653589793D1 * t437 * t446 / 0.8D1 + t518 
     #* t517 + t542 * t541
      t545 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t328
     #, x4)
      t546 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t328
     #, x4)
      t558 = t546 * t32
      t565 = (0.90D2 * t6 * 0.3141592653589793D1 * (-t545 + t337 * t546)
     # + 0.180D3 * t6 * t27 * t546) * t32 / 0.1440D4 - t65 * t558 * t69 
     #/ 0.16D2 - t65 * t558 * t98 / 0.8D1
      t566 = FJET(XB1, XB2, s, t325, 0.0D0, -t327, 0.0D0, 0.0D0, t565)
      t569 = t2 * x1 * x3
      t571 = t1 * t194
      t572 = x3 * s * t571
      t573 = t326 * s
      t574 = t573 * t430
      t575 = t573 * t571
      t576 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t328, x
     #4)
      t581 = FJET(XB1, XB2, s, t569, -t572, -t574, t575, 0.0D0, t65 * t5
     #76 * t32 * t98 / 0.8D1)
      t585 = t32 * t98
      t589 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t359, x
     #4)
      t594 = FJET(XB1, XB2, s, t360, 0.0D0, t363, 0.0D0, 0.0D0, -t6 * 0.
     #3141592653589793D1 * t589 * t378 / 0.16D2)
      t601 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t328, x
     #4)
      t606 = FJET(XB1, XB2, s, t575, -t574, -t572, t569, 0.0D0, t65 * t6
     #01 * t32 * t98 / 0.8D1)
      t613 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t359, x
     #4)
      t618 = FJET(XB1, XB2, s, t363, 0.0D0, t360, 0.0D0, 0.0D0, -t6 * 0.
     #3141592653589793D1 * t613 * t378 / 0.16D2)
      t625 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t631 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t646 = -t65 * t625 * t69 * t98 / 0.8D1 + (0.90D2 * t6 * 0.31415926
     #53589793D1 * (t211 * t625 - t631) + 0.180D3 * t6 * t27 * t625) * t
     #98 / 0.720D3 - t65 * t625 * t32 * t98 / 0.8D1
      t647 = FJET(XB1, XB2, s, -t195, t193, 0.0D0, 0.0D0, 0.0D0, t646)
      t649 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t654 = FJET(XB1, XB2, s, -t195, -t431, 0.0D0, -t428, t436, t65 * t
     #649 * t69 * t98 / 0.8D1)
      t661 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t328
     #, x4)
      t662 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t328
     #, x4)
      t674 = t662 * t32
      t681 = (0.90D2 * t6 * 0.3141592653589793D1 * (-t661 + t337 * t662)
     # + 0.180D3 * t6 * t27 * t662) * t32 / 0.1440D4 - t65 * t674 * t69 
     #/ 0.16D2 - t65 * t674 * t98 / 0.8D1
      t682 = FJET(XB1, XB2, s, -t327, 0.0D0, t325, 0.0D0, 0.0D0, t681)
      t684 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t328, x
     #4)
      t689 = FJET(XB1, XB2, s, -t572, t569, t575, -t574, 0.0D0, t65 * t6
     #84 * t32 * t98 / 0.8D1)
      t696 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t701 = FJET(XB1, XB2, s, -t431, -t195, -t428, 0.0D0, t436, t65 * t
     #696 * t69 * t98 / 0.8D1)
      t708 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t328, x
     #4)
      t713 = FJET(XB1, XB2, s, -t574, t575, t569, -t572, 0.0D0, t65 * t7
     #08 * t32 * t98 / 0.8D1)
      t720 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t725 = FJET(XB1, XB2, s, -t428, 0.0D0, -t431, -t195, t436, t65 * t
     #720 * t69 * t98 / 0.8D1)
      t732 = t566 * t565 + t581 * t3 * t5 * 0.3141592653589793D1 * t576 
     #* t585 / 0.8D1 - t594 * t3 * t383 * t589 * z * t387 / 0.16D2 + t60
     #6 * t3 * t5 * 0.3141592653589793D1 * t601 * t585 / 0.8D1 - t618 * 
     #t3 * t383 * t613 * z * t387 / 0.16D2 + t647 * t646 + t654 * t3 * t
     #5 * 0.3141592653589793D1 * t649 * t446 / 0.8D1 + t682 * t681 + t68
     #9 * t3 * t5 * 0.3141592653589793D1 * t684 * t585 / 0.8D1 + t701 * 
     #t3 * t5 * 0.3141592653589793D1 * t696 * t446 / 0.8D1 + t713 * t3 *
     # t5 * 0.3141592653589793D1 * t708 * t585 / 0.8D1 + t725 * t3 * t5 
     #* 0.3141592653589793D1 * t720 * t446 / 0.8D1
      rrqg2qght8s4em1 = t544 + t732

      end function



      doubleprecision function rrqg2qght8s4em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10D1
     #, x4)
      t8 = 0.3141592653589793D1 * t7
      t9 = 0.1D1 / x3
      t13 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #x4)
      t16 = 0.1D1 / x2
      t20 = 0.1D1 / x1
      t25 = t5 * lh
      t28 = rrqg2qgh82J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10D
     #1, x4)
      t29 = z ** 2
      t33 = Sin(x4 * 0.3141592653589793D1)
      t34 = t33 ** 2
      t36 = t1 ** 2
      t37 = t36 ** 2
      t40 = log(0.4D1 / t29 / z * t34 * t37)
      t49 = t6 * t8 * t9 / 0.16D2 + t6 * 0.3141592653589793D1 * (-t13 + 
     #t7) * t16 / 0.16D2 + t6 * t8 * t20 / 0.8D1 + (-0.180D3 * t7 * t3 *
     # t25 + 0.90D2 * (t28 - t40 * t7) * t3 * t5) * 0.3141592653589793D1
     # / 0.1440D4
      t50 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t49)
      t52 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10D
     #1, x4)
      t53 = 0.3141592653589793D1 * t52
      t57 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #x4)
      t69 = rrqg2qgh84J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10D
     #1, x4)
      t78 = t6 * t53 * t9 / 0.16D2 + t6 * 0.3141592653589793D1 * (-t57 +
     # t52) * t16 / 0.16D2 + t6 * t53 * t20 / 0.8D1 + (-0.180D3 * t52 * 
     #t3 * t25 + 0.90D2 * (t69 - t40 * t52) * t3 * t5) * 0.3141592653589
     #793D1 / 0.1440D4
      t79 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t78)
      t81 = t2 * x1
      t83 = t2 * (-0.1D1 + x1)
      t84 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1, 
     #x4)
      t86 = 0.3141592653589793D1 * t84 * t20
      t89 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t81, -t83, 0.0D0, -t6 * t86 
     #/ 0.8D1)
      t94 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1, 
     #x4)
      t96 = 0.3141592653589793D1 * t94 * t20
      t99 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t83, t81, 0.0D0, -t6 * t96 
     #/ 0.8D1)
      t104 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t105 = 0.3141592653589793D1 * t104
      t109 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t121 = rrqg2qgh81J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t130 = t6 * t105 * t9 / 0.16D2 + t6 * 0.3141592653589793D1 * (-t10
     #9 + t104) * t16 / 0.16D2 + t6 * t105 * t20 / 0.8D1 + (-0.180D3 * t
     #104 * t3 * t25 + 0.90D2 * (t121 - t40 * t104) * t3 * t5) * 0.31415
     #92653589793D1 / 0.1440D4
      t131 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t130)
      t133 = t2 * x3
      t134 = -0.1D1 + x3
      t135 = t2 * t134
      t136 = -t134
      t137 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t136
     #, x4)
      t139 = 0.3141592653589793D1 * t137 * t9
      t142 = FJET(XB1, XB2, s, 0.0D0, t133, 0.0D0, -t135, 0.0D0, -t6 * t
     #139 / 0.16D2)
      t147 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t136
     #, x4)
      t149 = 0.3141592653589793D1 * t147 * t9
      t152 = FJET(XB1, XB2, s, 0.0D0, -t135, 0.0D0, t133, 0.0D0, -t6 * t
     #149 / 0.16D2)
      t157 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t158 = 0.3141592653589793D1 * t157
      t162 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1,
     # x4)
      t174 = rrqg2qgh83J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10
     #D1, x4)
      t183 = t6 * t158 * t9 / 0.16D2 + t6 * 0.3141592653589793D1 * (-t16
     #2 + t157) * t16 / 0.16D2 + t6 * t158 * t20 / 0.8D1 + (-0.180D3 * t
     #157 * t3 * t25 + 0.90D2 * (t174 - t40 * t157) * t3 * t5) * 0.31415
     #92653589793D1 / 0.1440D4
      t184 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t183)
      t186 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t188 = 0.3141592653589793D1 * t186 * t20
      t191 = FJET(XB1, XB2, s, t81, -t83, 0.0D0, 0.0D0, 0.0D0, -t6 * t18
     #8 / 0.8D1)
      t196 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t136
     #, x4)
      t198 = 0.3141592653589793D1 * t196 * t9
      t201 = FJET(XB1, XB2, s, t133, 0.0D0, -t135, 0.0D0, 0.0D0, -t6 * t
     #198 / 0.16D2)
      t206 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, t136
     #, x4)
      t208 = 0.3141592653589793D1 * t206 * t9
      t211 = FJET(XB1, XB2, s, -t135, 0.0D0, t133, 0.0D0, 0.0D0, -t6 * t
     #208 / 0.16D2)
      t216 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.10D1,
     # x4)
      t218 = 0.3141592653589793D1 * t216 * t20
      t221 = FJET(XB1, XB2, s, -t83, t81, 0.0D0, 0.0D0, 0.0D0, -t6 * t21
     #8 / 0.8D1)
      rrqg2qght8s4em2 = t50 * t49 + t79 * t78 - t89 * t3 * t5 * t86 / 0.
     #8D1 - t99 * t3 * t5 * t96 / 0.8D1 + t131 * t130 - t142 * t3 * t5 *
     # t139 / 0.16D2 - t152 * t3 * t5 * t149 / 0.16D2 + t184 * t183 - t1
     #91 * t3 * t5 * t188 / 0.8D1 - t201 * t3 * t5 * t198 / 0.16D2 - t21
     #1 * t3 * t5 * t208 / 0.16D2 - t221 * t3 * t5 * t218 / 0.8D1

      end function



      doubleprecision function rrqg2qght8s4em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = rrqg2qgh82J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10D1
     #, x4)
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t6 * 0.314
     #1592653589793D1 * t7 / 0.16D2)
      t13 = t5 * 0.3141592653589793D1
      t16 = rrqg2qgh84J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10D
     #1, x4)
      t20 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t6 * 0.314
     #1592653589793D1 * t16 / 0.16D2)
      t24 = rrqg2qgh81J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10D
     #1, x4)
      t28 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t6 * 0.314
     #1592653589793D1 * t24 / 0.16D2)
      t32 = rrqg2qgh83J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.10D
     #1, x4)
      t36 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t6 * 0.314
     #1592653589793D1 * t32 / 0.16D2)
      rrqg2qght8s4em3 = t11 * t3 * t13 * t7 / 0.16D2 + t20 * t3 * t13 * 
     #t16 / 0.16D2 + t28 * t3 * t13 * t24 / 0.16D2 + t36 * t3 * t13 * t3
     #2 / 0.16D2

      end function



      doubleprecision function rrqg2qght8s4em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh81J1
      doubleprecision rrqg2qgh81J2
      doubleprecision rrqg2qgh81J3
      doubleprecision rrqg2qgh81J4
      doubleprecision rrqg2qgh81J5
      doubleprecision rrqg2qgh81J6
      doubleprecision rrqg2qgh82J1
      doubleprecision rrqg2qgh82J2
      doubleprecision rrqg2qgh82J3
      doubleprecision rrqg2qgh82J4
      doubleprecision rrqg2qgh82J5
      doubleprecision rrqg2qgh82J6
      doubleprecision rrqg2qgh82J7
      doubleprecision rrqg2qgh83J1
      doubleprecision rrqg2qgh83J2
      doubleprecision rrqg2qgh83J3
      doubleprecision rrqg2qgh83J4
      doubleprecision rrqg2qgh83J5
      doubleprecision rrqg2qgh83J6
      doubleprecision rrqg2qgh83J7
      doubleprecision rrqg2qgh84J1
      doubleprecision rrqg2qgh84J2
      doubleprecision rrqg2qgh84J3
      doubleprecision rrqg2qgh84J4
      doubleprecision rrqg2qgh84J5
      doubleprecision rrqg2qgh84J6
      rrqg2qght8s4em4 = 0.0D0

      end function
  
 

      doubleprecision function rrqg2qgh81J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + x1 * t1
      t8 = x3 * (0.1D1 - x2)
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t23 = 0.1D1 - x1
      t26 = s - t2 * x1 / t4 * (t8 * t4 + x2 * t10 - 0.2D1 * t13 * t17) 
     #- t2 * t23 * x3
      t27 = s ** 2
      t28 = t27 * s
      t29 = t26 * t28
      t30 = t1 ** 2
      t31 = t30 * t1
      t32 = t29 * t31
      t33 = t23 ** 2
      t34 = t33 * t23
      t35 = t10 ** 2
      t37 = t34 * t35 * x3
      t40 = t1 * t23
      t44 = t27 ** 2
      t45 = t30 ** 2
      t46 = t44 * t45
      t47 = t33 ** 2
      t49 = x3 ** 2
      t54 = t49 * x3
      t58 = t44 * t31
      t61 = t44 * t30
      t63 = t33 * t10 * x3
      t67 = t34 * t10 * t49
      t89 = t35 * t10
      t106 = 0.54D2 * t32 * t37 + 0.9D1 * t29 * t40 * t10 - 0.18D2 * t46
     # * t47 * t35 * t49 - 0.9D1 * t46 * t47 * t10 * t54 - 0.36D2 * t58 
     #* t37 - 0.27D2 * t61 * t63 - 0.27D2 * t58 * t67 + 0.18D2 * t29 * t
     #30 * t33 * t35 + 0.63D2 * t32 * t67 - 0.36D2 * t28 * t31 * t34 * t
     #54 * t26 - 0.36D2 * t29 * t40 * x3 - 0.9D1 * t44 * t1 * t23 * t10 
     #+ 0.18D2 * t29 * t31 * t34 * t89 - 0.18D2 * t58 * t34 * t89 - 0.18
     #D2 * t46 * t47 * t89 * x3 - 0.18D2 * t61 * t33 * t35 + 0.36D2 * t2
     #9 * t30 * t63
      rrqg2qgh81J1 = -wd * t106 / t26 / s / z / 0.3141592653589793D1 / 0
     #.9D1

      end function
  
   
 

      doubleprecision function rrqg2qgh81J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + x1 * t1
      t5 = 0.1D1 / t4
      t6 = x1 * t5
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t23 = 0.1D1 - x1
      t24 = t23 * x3
      t26 = s - t2 * t6 * (t8 * t4 + x2 * t10 - t19) - t2 * t24
      t27 = s ** 2
      t28 = t27 * s
      t29 = t26 * t28
      t30 = t1 ** 2
      t31 = t30 * t1
      t32 = t29 * t31
      t33 = t23 ** 2
      t34 = t33 * t23
      t35 = t10 ** 2
      t36 = t34 * t35
      t37 = t36 * x3
      t40 = t1 * t23
      t44 = t27 ** 2
      t45 = t30 ** 2
      t46 = t44 * t45
      t47 = t33 ** 2
      t49 = x3 ** 2
      t54 = t49 * x3
      t58 = t44 * t31
      t61 = t44 * t30
      t62 = t33 * t10
      t63 = t62 * x3
      t67 = t34 * t10 * t49
      t70 = t30 * t33
      t76 = t28 * t31
      t80 = 0.36D2 * t76 * t34 * t54 * t26
      t83 = 0.36D2 * t29 * t40 * x3
      t89 = t35 * t10
      t92 = 0.18D2 * t29 * t31 * t34 * t89
      t95 = 0.18D2 * t58 * t34 * t89
      t99 = 0.18D2 * t46 * t47 * t89 * x3
      t106 = 0.54D2 * t32 * t37 + 0.9D1 * t29 * t40 * t10 - 0.18D2 * t46
     # * t47 * t35 * t49 - 0.9D1 * t46 * t47 * t10 * t54 - 0.36D2 * t58 
     #* t37 - 0.27D2 * t61 * t63 - 0.27D2 * t58 * t67 + 0.18D2 * t29 * t
     #70 * t35 + 0.63D2 * t32 * t67 - t80 - t83 - 0.9D1 * t44 * t1 * t23
     # * t10 + t92 - t95 - t99 - 0.18D2 * t61 * t33 * t35 + 0.36D2 * t29
     # * t30 * t63
      t109 = x1 ** 2
      t111 = t4 ** 2
      t112 = 0.1D1 / t111
      t116 = t10 * t7 * t4 + x2 * x3 + t19
      t117 = t116 ** 2
      t118 = t112 * t117
      t124 = t5 * t116
      t130 = t35 * x1 * t124
      t135 = t10 * t109 * t118
      t166 = t95 + t99 - t92 + t80 - 0.18D2 * t46 * t62 * x3 * t109 * t1
     #18 + 0.36D2 * t29 * t30 * x1 * t124 * t24 - 0.18D2 * t58 * t33 * t
     #130 - 0.18D2 * t58 * t23 * t135 + t83 - 0.18D2 * t46 * t36 * x3 * 
     #x1 * t124 - 0.36D2 * t76 * t109 * t112 * t117 * t26 * t24 - 0.72D2
     # * t29 * t70 * t49 + 0.18D2 * t29 * t31 * t33 * t130 - 0.36D2 * t7
     #6 * t6 * t116 * t26 * t33 * t49 + 0.18D2 * t29 * t31 * t23 * t135
      rrqg2qgh81J2 = -(wd * t106 + wd * t166) / t26 / s / z / 0.31415926
     #53589793D1 / 0.9D1

      end function
  
   
 

      doubleprecision function rrqg2qgh81J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + x1 * t1
      t5 = 0.1D1 / t4
      t6 = x1 * t5
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t23 = 0.1D1 - x1
      t24 = t23 * x3
      t26 = s - t2 * t6 * (t8 * t4 + x2 * t10 - t19) - t2 * t24
      t27 = s ** 2
      t28 = t27 * s
      t29 = t26 * t28
      t30 = t1 ** 2
      t31 = t30 * t1
      t32 = t29 * t31
      t33 = t23 ** 2
      t34 = t33 * t23
      t35 = t10 ** 2
      t36 = t34 * t35
      t37 = t36 * x3
      t40 = t1 * t23
      t44 = t27 ** 2
      t45 = t30 ** 2
      t46 = t44 * t45
      t47 = t33 ** 2
      t49 = x3 ** 2
      t54 = t49 * x3
      t58 = t44 * t31
      t61 = t44 * t30
      t62 = t33 * t10
      t63 = t62 * x3
      t67 = t34 * t10 * t49
      t70 = t30 * t33
      t76 = t28 * t31
      t80 = 0.36D2 * t76 * t34 * t54 * t26
      t83 = 0.36D2 * t29 * t40 * x3
      t89 = t35 * t10
      t92 = 0.18D2 * t29 * t31 * t34 * t89
      t95 = 0.18D2 * t58 * t34 * t89
      t99 = 0.18D2 * t46 * t47 * t89 * x3
      t106 = 0.54D2 * t32 * t37 + 0.9D1 * t29 * t40 * t10 - 0.18D2 * t46
     # * t47 * t35 * t49 - 0.9D1 * t46 * t47 * t10 * t54 - 0.36D2 * t58 
     #* t37 - 0.27D2 * t61 * t63 - 0.27D2 * t58 * t67 + 0.18D2 * t29 * t
     #70 * t35 + 0.63D2 * t32 * t67 - t80 - t83 - 0.9D1 * t44 * t1 * t23
     # * t10 + t92 - t95 - t99 - 0.18D2 * t61 * t33 * t35 + 0.36D2 * t29
     # * t30 * t63
      t109 = x1 ** 2
      t111 = t4 ** 2
      t112 = 0.1D1 / t111
      t116 = t10 * t7 * t4 + x2 * x3 + t19
      t117 = t116 ** 2
      t118 = t112 * t117
      t124 = t5 * t116
      t130 = t35 * x1 * t124
      t135 = t10 * t109 * t118
      t166 = t95 + t99 - t92 + t80 - 0.18D2 * t46 * t62 * x3 * t109 * t1
     #18 + 0.36D2 * t29 * t30 * x1 * t124 * t24 - 0.18D2 * t58 * t33 * t
     #130 - 0.18D2 * t58 * t23 * t135 + t83 - 0.18D2 * t46 * t36 * x3 * 
     #x1 * t124 - 0.36D2 * t76 * t109 * t112 * t117 * t26 * t24 - 0.72D2
     # * t29 * t70 * t49 + 0.18D2 * t29 * t31 * t33 * t130 - 0.36D2 * t7
     #6 * t6 * t116 * t26 * t33 * t49 + 0.18D2 * t29 * t31 * t23 * t135
      rrqg2qgh81J3 = -(wd * t106 + wd * t166) / t26 / s / z / 0.31415926
     #53589793D1 / 0.9D1

      end function
  
   
 

      doubleprecision function rrqg2qgh81J4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + x1 * t1
      t5 = 0.1D1 / t4
      t6 = x1 * t5
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t23 = 0.1D1 - x1
      t24 = t23 * x3
      t26 = s - t2 * t6 * (t8 * t4 + x2 * t10 - t19) - t2 * t24
      t27 = s ** 2
      t28 = t27 * s
      t29 = t26 * t28
      t30 = t1 ** 2
      t31 = t30 * t1
      t32 = t29 * t31
      t33 = t23 ** 2
      t34 = t33 * t23
      t35 = t10 ** 2
      t36 = t34 * t35
      t37 = t36 * x3
      t40 = t1 * t23
      t44 = t27 ** 2
      t45 = t30 ** 2
      t46 = t44 * t45
      t47 = t33 ** 2
      t49 = x3 ** 2
      t54 = t49 * x3
      t58 = t44 * t31
      t61 = t44 * t30
      t62 = t33 * t10
      t63 = t62 * x3
      t67 = t34 * t10 * t49
      t70 = t30 * t33
      t76 = t28 * t31
      t80 = 0.36D2 * t76 * t34 * t54 * t26
      t83 = 0.36D2 * t29 * t40 * x3
      t89 = t35 * t10
      t92 = 0.18D2 * t29 * t31 * t34 * t89
      t95 = 0.18D2 * t58 * t34 * t89
      t99 = 0.18D2 * t46 * t47 * t89 * x3
      t106 = 0.54D2 * t32 * t37 + 0.9D1 * t29 * t40 * t10 - 0.18D2 * t46
     # * t47 * t35 * t49 - 0.9D1 * t46 * t47 * t10 * t54 - 0.36D2 * t58 
     #* t37 - 0.27D2 * t61 * t63 - 0.27D2 * t58 * t67 + 0.18D2 * t29 * t
     #70 * t35 + 0.63D2 * t32 * t67 - t80 - t83 - 0.9D1 * t44 * t1 * t23
     # * t10 + t92 - t95 - t99 - 0.18D2 * t61 * t33 * t35 + 0.36D2 * t29
     # * t30 * t63
      t109 = x1 ** 2
      t111 = t4 ** 2
      t112 = 0.1D1 / t111
      t116 = t10 * t7 * t4 + x2 * x3 + t19
      t117 = t116 ** 2
      t118 = t112 * t117
      t124 = t5 * t116
      t130 = t35 * x1 * t124
      t135 = t10 * t109 * t118
      t166 = t95 + t99 - t92 + t80 - 0.18D2 * t46 * t62 * x3 * t109 * t1
     #18 + 0.36D2 * t29 * t30 * x1 * t124 * t24 - 0.18D2 * t58 * t33 * t
     #130 - 0.18D2 * t58 * t23 * t135 + t83 - 0.18D2 * t46 * t36 * x3 * 
     #x1 * t124 - 0.36D2 * t76 * t109 * t112 * t117 * t26 * t24 - 0.72D2
     # * t29 * t70 * t49 + 0.18D2 * t29 * t31 * t33 * t130 - 0.36D2 * t7
     #6 * t6 * t116 * t26 * t33 * t49 + 0.18D2 * t29 * t31 * t23 * t135
      rrqg2qgh81J4 = -(wd * t106 + wd * t166) / t26 / s / z / 0.31415926
     #53589793D1 / 0.9D1

      end function
  
   
 

      doubleprecision function rrqg2qgh81J5
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + x1 * t1
      t5 = 0.1D1 / t4
      t6 = x1 * t5
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t23 = 0.1D1 - x1
      t24 = t23 * x3
      t26 = s - t2 * t6 * (t8 * t4 + x2 * t10 - t19) - t2 * t24
      t27 = s ** 2
      t28 = t27 * s
      t29 = t26 * t28
      t30 = t1 ** 2
      t31 = t30 * t1
      t32 = t29 * t31
      t33 = t23 ** 2
      t34 = t33 * t23
      t35 = t10 ** 2
      t36 = t34 * t35
      t37 = t36 * x3
      t40 = t1 * t23
      t44 = t27 ** 2
      t45 = t30 ** 2
      t46 = t44 * t45
      t47 = t33 ** 2
      t49 = x3 ** 2
      t54 = t49 * x3
      t58 = t44 * t31
      t61 = t44 * t30
      t62 = t33 * t10
      t63 = t62 * x3
      t67 = t34 * t10 * t49
      t70 = t30 * t33
      t76 = t28 * t31
      t80 = 0.36D2 * t76 * t34 * t54 * t26
      t83 = 0.36D2 * t29 * t40 * x3
      t89 = t35 * t10
      t92 = 0.18D2 * t29 * t31 * t34 * t89
      t95 = 0.18D2 * t58 * t34 * t89
      t99 = 0.18D2 * t46 * t47 * t89 * x3
      t106 = 0.54D2 * t32 * t37 + 0.9D1 * t29 * t40 * t10 - 0.18D2 * t46
     # * t47 * t35 * t49 - 0.9D1 * t46 * t47 * t10 * t54 - 0.36D2 * t58 
     #* t37 - 0.27D2 * t61 * t63 - 0.27D2 * t58 * t67 + 0.18D2 * t29 * t
     #70 * t35 + 0.63D2 * t32 * t67 - t80 - t83 - 0.9D1 * t44 * t1 * t23
     # * t10 + t92 - t95 - t99 - 0.18D2 * t61 * t33 * t35 + 0.36D2 * t29
     # * t30 * t63
      t109 = x1 ** 2
      t111 = t4 ** 2
      t112 = 0.1D1 / t111
      t116 = t10 * t7 * t4 + x2 * x3 + t19
      t117 = t116 ** 2
      t118 = t112 * t117
      t124 = t5 * t116
      t130 = t35 * x1 * t124
      t135 = t10 * t109 * t118
      t166 = t95 + t99 - t92 + t80 - 0.18D2 * t46 * t62 * x3 * t109 * t1
     #18 + 0.36D2 * t29 * t30 * x1 * t124 * t24 - 0.18D2 * t58 * t33 * t
     #130 - 0.18D2 * t58 * t23 * t135 + t83 - 0.18D2 * t46 * t36 * x3 * 
     #x1 * t124 - 0.36D2 * t76 * t109 * t112 * t117 * t26 * t24 - 0.72D2
     # * t29 * t70 * t49 + 0.18D2 * t29 * t31 * t33 * t130 - 0.36D2 * t7
     #6 * t6 * t116 * t26 * t33 * t49 + 0.18D2 * t29 * t31 * t23 * t135
      rrqg2qgh81J5 = -(wd * t106 + wd * t166) / t26 / s / z / 0.31415926
     #53589793D1 / 0.9D1

      end function
  
   
 

      doubleprecision function rrqg2qgh81J6
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t5 = t4 * t3
      t6 = t2 * t5
      t7 = 0.1D1 - x1
      t8 = t7 ** 2
      t9 = t8 * t7
      t10 = 0.1D1 - x3
      t11 = t10 ** 2
      t12 = t11 * t10
      t16 = t4 ** 2
      t17 = t2 * t16
      t18 = t8 ** 2
      t23 = s * t3
      t25 = z + x1 * t3
      t26 = 0.1D1 / t25
      t27 = x1 * t26
      t28 = 0.1D1 - x2
      t29 = x3 * t28
      t33 = cos(x4 * 0.3141592653589793D1)
      t37 = Sqrt(t29 * t25 * x2 * t10)
      t39 = 0.2D1 * t33 * t37
      t43 = x3 * t7
      t45 = s - t23 * t27 * (t29 * t25 + x2 * t10 - t39) - t23 * t43
      t46 = s * t1
      t47 = t45 * t46
      t52 = t46 * t5
      t53 = x3 ** 2
      t61 = x1 ** 2
      t63 = t25 ** 2
      t64 = 0.1D1 / t63
      t68 = t10 * t28 * t25 + x2 * x3 + t39
      t69 = t68 ** 2
      t70 = t64 * t69
      t76 = t26 * t68
      t82 = t11 * x1 * t76
      t87 = t10 * t61 * t70
      t124 = 0.18D2 * t6 * t9 * t12 + 0.18D2 * t17 * t18 * t12 * x3 - 0.
     #18D2 * t47 * t5 * t9 * t12 + 0.36D2 * t52 * t9 * t53 * x3 * t45 - 
     #0.18D2 * t17 * t8 * t10 * x3 * t61 * t70 + 0.36D2 * t47 * t4 * x1 
     #* t76 * t43 - 0.18D2 * t6 * t8 * t82 - 0.18D2 * t6 * t7 * t87 + 0.
     #36D2 * t47 * t3 * t7 * x3 - 0.18D2 * t17 * t9 * t11 * x3 * x1 * t7
     #6 - 0.36D2 * t52 * t61 * t64 * t69 * t45 * t43 - 0.72D2 * t47 * t4
     # * t8 * t53 + 0.18D2 * t47 * t5 * t8 * t82 - 0.36D2 * t52 * t27 * 
     #t68 * t45 * t8 * t53 + 0.18D2 * t47 * t5 * t7 * t87
      rrqg2qgh81J6 = -wd * t124 / t45 / s / z / 0.3141592653589793D1 / 0
     #.9D1

      end function
  
   
 

      doubleprecision function rrqg2qgh82J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + x1 * t1
      t7 = 0.1D1 - x3
      t8 = 0.1D1 - x2
      t13 = cos(x4 * 0.3141592653589793D1)
      t18 = Sqrt(x3 * t8 * t4 * x2 * t7)
      t24 = 0.1D1 - x1
      t27 = s - t2 * x1 / t4 * (t7 * t8 * t4 + x2 * x3 + 0.2D1 * t13 * t
     #18) - t2 * t24 * t7
      t28 = s ** 2
      t30 = t27 * t28 * s
      t31 = t1 ** 2
      t32 = t31 * t1
      t33 = t24 ** 2
      t34 = t33 * t24
      t36 = t7 ** 2
      t40 = t30 * t32
      t42 = x3 ** 2
      rrqg2qgh82J1 = -wd * (0.8D1 * t30 * t32 * t34 * t36 * t7 + 0.8D1 *
     # t40 * t34 * t7 * t42 + 0.8D1 * t30 * t1 * t24 * t7 + 0.8D1 * t40 
     #* t34 * t36 * x3 - 0.8D1 * t30 * t31 * t33 * t7 * x3) / t27 / s / 
     #z / 0.3141592653589793D1 / 0.9D1

      end function
  
   
 

      doubleprecision function rrqg2qgh82J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + x1 * t1
      t7 = 0.1D1 - x3
      t8 = 0.1D1 - x2
      t13 = cos(x4 * 0.3141592653589793D1)
      t14 = x3 * t8
      t18 = Sqrt(t14 * t4 * x2 * t7)
      t20 = 0.2D1 * t13 * t18
      t21 = t7 * t8 * t4 + x2 * x3 + t20
      t24 = 0.1D1 - x1
      t27 = s - t2 * x1 / t4 * t21 - t2 * t24 * t7
      t28 = s ** 2
      t30 = t27 * t28 * s
      t31 = t1 ** 2
      t32 = t31 * t1
      t33 = t24 ** 2
      t34 = t33 * t24
      t36 = t7 ** 2
      t39 = t30 * t32 * t34 * t36 * t7
      t40 = t30 * t32
      t42 = x3 ** 2
      t44 = t40 * t34 * t7 * t42
      t47 = t30 * t1 * t24 * t7
      t50 = t40 * t34 * t36 * x3
      t54 = t30 * t31 * t33 * t7 * x3
      t67 = x1 ** 2
      t70 = t4 ** 2
      rrqg2qgh82J2 = -(wd * (0.8D1 * t39 + 0.8D1 * t44 + 0.8D1 * t47 + 0
     #.8D1 * t50 - 0.8D1 * t54) + wd * (0.16D2 * t30 * t31 * t33 * t36 -
     # 0.16D2 * t44 - 0.8D1 * t39 - 0.8D1 * t47 + 0.16D2 * t54 - 0.16D2 
     #* t50 - 0.4D1 * t30 * t32 * t67 / t70 * (t14 * t4 + x2 * t7 - t20)
     # * t24 * x3 * t21)) / t27 / s / z / 0.3141592653589793D1 / 0.9D1

      end function
  
   
 

      doubleprecision function rrqg2qgh82J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + x1 * t1
      t5 = 0.1D1 / t4
      t7 = 0.1D1 - x3
      t8 = 0.1D1 - x2
      t13 = cos(x4 * 0.3141592653589793D1)
      t14 = x3 * t8
      t18 = Sqrt(t14 * t4 * x2 * t7)
      t20 = 0.2D1 * t13 * t18
      t21 = t7 * t8 * t4 + x2 * x3 + t20
      t24 = 0.1D1 - x1
      t27 = s - t2 * x1 * t5 * t21 - t2 * t24 * t7
      t28 = s ** 2
      t30 = t27 * t28 * s
      t31 = t1 ** 2
      t32 = t31 * t1
      t33 = t24 ** 2
      t34 = t33 * t24
      t36 = t7 ** 2
      t39 = t30 * t32 * t34 * t36 * t7
      t40 = t30 * t32
      t42 = x3 ** 2
      t44 = t40 * t34 * t7 * t42
      t47 = t30 * t1 * t24 * t7
      t50 = t40 * t34 * t36 * x3
      t54 = t30 * t31 * t33 * t7 * x3
      t67 = x1 ** 2
      t69 = t30 * t32 * t67
      t70 = t4 ** 2
      t71 = 0.1D1 / t70
      t74 = t14 * t4 + x2 * t7 - t20
      t76 = t24 * x3
      t80 = 0.4D1 * t69 * t71 * t74 * t76 * t21
      t83 = t28 ** 2
      t87 = t5 * t74
      t108 = t74 ** 2
      rrqg2qgh82J3 = -(wd * (0.8D1 * t39 + 0.8D1 * t44 + 0.8D1 * t47 + 0
     #.8D1 * t50 - 0.8D1 * t54) + wd * (0.16D2 * t30 * t31 * t33 * t36 -
     # 0.16D2 * t44 - 0.8D1 * t39 - 0.8D1 * t47 + 0.16D2 * t54 - 0.16D2 
     #* t50 - t80) + wd * (-0.4D1 * t83 * t32 * t33 * t42 * x1 * t87 + 0
     #.4D1 * t83 * t31 * t24 * x3 * x1 * t87 + 0.8D1 * t30 * t32 * x1 * 
     #t87 * t33 * t42 - t80 - 0.4D1 * t30 * t31 * x1 * t87 * t76 + 0.4D1
     # * t69 * t71 * t108 * t76)) / t27 / s / z / 0.3141592653589793D1 /
     # 0.9D1

      end function
  
   
 

      doubleprecision function rrqg2qgh82J4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + x1 * t1
      t5 = 0.1D1 / t4
      t7 = 0.1D1 - x3
      t8 = 0.1D1 - x2
      t13 = cos(x4 * 0.3141592653589793D1)
      t14 = x3 * t8
      t18 = Sqrt(t14 * t4 * x2 * t7)
      t20 = 0.2D1 * t13 * t18
      t21 = t7 * t8 * t4 + x2 * x3 + t20
      t24 = 0.1D1 - x1
      t27 = s - t2 * x1 * t5 * t21 - t2 * t24 * t7
      t28 = s ** 2
      t30 = t27 * t28 * s
      t31 = t1 ** 2
      t32 = t31 * t1
      t33 = t24 ** 2
      t34 = t33 * t24
      t36 = t7 ** 2
      t39 = t30 * t32 * t34 * t36 * t7
      t40 = t30 * t32
      t42 = x3 ** 2
      t44 = t40 * t34 * t7 * t42
      t47 = t30 * t1 * t24 * t7
      t50 = t40 * t34 * t36 * x3
      t54 = t30 * t31 * t33 * t7 * x3
      t67 = x1 ** 2
      t69 = t30 * t32 * t67
      t70 = t4 ** 2
      t71 = 0.1D1 / t70
      t74 = t14 * t4 + x2 * t7 - t20
      t76 = t24 * x3
      t80 = 0.4D1 * t69 * t71 * t74 * t76 * t21
      t83 = t28 ** 2
      t87 = t5 * t74
      t108 = t74 ** 2
      rrqg2qgh82J4 = -(wd * (0.8D1 * t39 + 0.8D1 * t44 + 0.8D1 * t47 + 0
     #.8D1 * t50 - 0.8D1 * t54) + wd * (0.16D2 * t30 * t31 * t33 * t36 -
     # 0.16D2 * t44 - 0.8D1 * t39 - 0.8D1 * t47 + 0.16D2 * t54 - 0.16D2 
     #* t50 - t80) + wd * (-0.4D1 * t83 * t32 * t33 * t42 * x1 * t87 + 0
     #.4D1 * t83 * t31 * t24 * x3 * x1 * t87 + 0.8D1 * t30 * t32 * x1 * 
     #t87 * t33 * t42 - t80 - 0.4D1 * t30 * t31 * x1 * t87 * t76 + 0.4D1
     # * t69 * t71 * t108 * t76)) / t27 / s / z / 0.3141592653589793D1 /
     # 0.9D1

      end function
  
   
 

      doubleprecision function rrqg2qgh82J5
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + x1 * t1
      t5 = 0.1D1 / t4
      t7 = 0.1D1 - x3
      t8 = 0.1D1 - x2
      t13 = cos(x4 * 0.3141592653589793D1)
      t14 = x3 * t8
      t18 = Sqrt(t14 * t4 * x2 * t7)
      t20 = 0.2D1 * t13 * t18
      t21 = t7 * t8 * t4 + x2 * x3 + t20
      t24 = 0.1D1 - x1
      t27 = s - t2 * x1 * t5 * t21 - t2 * t24 * t7
      t28 = s ** 2
      t30 = t27 * t28 * s
      t31 = t1 ** 2
      t32 = t31 * t1
      t33 = t24 ** 2
      t34 = t33 * t24
      t36 = t7 ** 2
      t39 = t30 * t32 * t34 * t36 * t7
      t40 = t30 * t32
      t42 = x3 ** 2
      t44 = t40 * t34 * t7 * t42
      t47 = t30 * t1 * t24 * t7
      t50 = t40 * t34 * t36 * x3
      t54 = t30 * t31 * t33 * t7 * x3
      t67 = x1 ** 2
      t69 = t30 * t32 * t67
      t70 = t4 ** 2
      t71 = 0.1D1 / t70
      t74 = t14 * t4 + x2 * t7 - t20
      t76 = t24 * x3
      t80 = 0.4D1 * t69 * t71 * t74 * t76 * t21
      t83 = t28 ** 2
      t87 = t5 * t74
      t108 = t74 ** 2
      rrqg2qgh82J5 = -(wd * (0.8D1 * t39 + 0.8D1 * t44 + 0.8D1 * t47 + 0
     #.8D1 * t50 - 0.8D1 * t54) + wd * (0.16D2 * t30 * t31 * t33 * t36 -
     # 0.16D2 * t44 - 0.8D1 * t39 - 0.8D1 * t47 + 0.16D2 * t54 - 0.16D2 
     #* t50 - t80) + wd * (-0.4D1 * t83 * t32 * t33 * t42 * x1 * t87 + 0
     #.4D1 * t83 * t31 * t24 * x3 * x1 * t87 + 0.8D1 * t30 * t32 * x1 * 
     #t87 * t33 * t42 - t80 - 0.4D1 * t30 * t31 * x1 * t87 * t76 + 0.4D1
     # * t69 * t71 * t108 * t76)) / t27 / s / z / 0.3141592653589793D1 /
     # 0.9D1

      end function
  
   
 

      doubleprecision function rrqg2qgh82J6
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + x1 * t1
      t5 = 0.1D1 / t4
      t7 = 0.1D1 - x3
      t8 = 0.1D1 - x2
      t13 = cos(x4 * 0.3141592653589793D1)
      t14 = x3 * t8
      t18 = Sqrt(t14 * t4 * x2 * t7)
      t20 = 0.2D1 * t13 * t18
      t21 = t7 * t8 * t4 + x2 * x3 + t20
      t24 = 0.1D1 - x1
      t27 = s - t2 * x1 * t5 * t21 - t2 * t24 * t7
      t28 = s ** 2
      t30 = t27 * t28 * s
      t31 = t1 ** 2
      t32 = t24 ** 2
      t34 = t7 ** 2
      t38 = t31 * t1
      t39 = t30 * t38
      t40 = t32 * t24
      t42 = x3 ** 2
      t64 = x1 ** 2
      t66 = t30 * t38 * t64
      t67 = t4 ** 2
      t68 = 0.1D1 / t67
      t71 = t14 * t4 + x2 * t7 - t20
      t73 = t24 * x3
      t77 = 0.4D1 * t66 * t68 * t71 * t73 * t21
      t80 = t28 ** 2
      t84 = t5 * t71
      t105 = t71 ** 2
      rrqg2qgh82J6 = -(wd * (0.16D2 * t30 * t31 * t32 * t34 - 0.16D2 * t
     #39 * t40 * t7 * t42 - 0.8D1 * t30 * t38 * t40 * t34 * t7 - 0.8D1 *
     # t30 * t1 * t24 * t7 + 0.16D2 * t30 * t31 * t32 * t7 * x3 - 0.16D2
     # * t39 * t40 * t34 * x3 - t77) + wd * (-0.4D1 * t80 * t38 * t32 * 
     #t42 * x1 * t84 + 0.4D1 * t80 * t31 * t24 * x3 * x1 * t84 + 0.8D1 *
     # t30 * t38 * x1 * t84 * t32 * t42 - t77 - 0.4D1 * t30 * t31 * x1 *
     # t84 * t73 + 0.4D1 * t66 * t68 * t105 * t73)) / t27 / s / z / 0.31
     #41592653589793D1 / 0.9D1

      end function
  
   
 

      doubleprecision function rrqg2qgh82J7
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t5 = t4 * t3
      t7 = 0.1D1 - x1
      t8 = t7 ** 2
      t10 = x3 ** 2
      t13 = z + x1 * t3
      t14 = 0.1D1 / t13
      t15 = 0.1D1 - x2
      t16 = x3 * t15
      t18 = 0.1D1 - x3
      t21 = cos(x4 * 0.3141592653589793D1)
      t25 = Sqrt(t16 * t13 * x2 * t18)
      t27 = 0.2D1 * t21 * t25
      t28 = t16 * t13 + x2 * t18 - t27
      t29 = t14 * t28
      t39 = s * t3
      t44 = t18 * t15 * t13 + x2 * x3 + t27
      t49 = s - t39 * x1 * t14 * t44 - t39 * t7 * t18
      t51 = t49 * t1 * s
      t58 = x1 ** 2
      t60 = t51 * t5 * t58
      t61 = t13 ** 2
      t62 = 0.1D1 / t61
      t64 = x3 * t7
      t74 = t28 ** 2
      rrqg2qgh82J7 = -wd * (-0.4D1 * t2 * t5 * t8 * t10 * x1 * t29 + 0.4
     #D1 * t2 * t4 * t7 * x3 * x1 * t29 + 0.8D1 * t51 * t5 * x1 * t29 * 
     #t8 * t10 - 0.4D1 * t60 * t62 * t28 * t64 * t44 - 0.4D1 * t51 * t4 
     #* x1 * t29 * t64 + 0.4D1 * t60 * t62 * t74 * t64) / t49 / s / z / 
     #0.3141592653589793D1 / 0.9D1

      end function
  
   
 

      doubleprecision function rrqg2qgh83J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + x1 * t1
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t20 = t8 * t4 + x2 * t10 - t19
      t21 = x1 / t4 * t20
      t26 = s - t2 * t21 - t2 * (0.1D1 - x1) * x3
      t27 = s ** 2
      t29 = t26 * t27 * s
      t30 = t1 ** 2
      t32 = t29 * t30 * t1
      t33 = x1 ** 2
      t35 = t4 ** 2
      t38 = t33 * x1 / t35 / t4
      t42 = t10 * t7 * t4 + x2 * x3 + t19
      t43 = t42 ** 2
      t49 = t20 ** 2
      rrqg2qgh83J1 = -wd * (0.8D1 * t32 * t38 * t20 * t43 + 0.8D1 * t29 
     #* t1 * t21 + 0.8D1 * t32 * t38 * t49 * t42 + 0.8D1 * t32 * t38 * t
     #49 * t20 - 0.8D1 * t29 * t30 * t33 / t35 * t20 * t42) / t26 / s / 
     #z / 0.3141592653589793D1 / 0.9D1

      end function
  
   
 

      doubleprecision function rrqg2qgh83J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + x1 * t1
      t5 = 0.1D1 / t4
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t20 = t8 * t4 + x2 * t10 - t19
      t21 = x1 * t5 * t20
      t23 = 0.1D1 - x1
      t26 = s - t2 * t21 - t2 * t23 * x3
      t27 = s ** 2
      t29 = t26 * t27 * s
      t30 = t1 ** 2
      t31 = t30 * t1
      t32 = t29 * t31
      t33 = x1 ** 2
      t35 = t4 ** 2
      t38 = t33 * x1 / t35 / t4
      t42 = t10 * t7 * t4 + x2 * x3 + t19
      t43 = t42 ** 2
      t46 = t32 * t38 * t20 * t43
      t48 = t29 * t1 * t21
      t49 = t20 ** 2
      t52 = t32 * t38 * t49 * t42
      t55 = t32 * t38 * t49 * t20
      t56 = t29 * t30
      t58 = t33 / t35
      t61 = t56 * t58 * t20 * t42
      t73 = t23 ** 2
      rrqg2qgh83J2 = -(wd * (0.8D1 * t46 + 0.8D1 * t48 + 0.8D1 * t52 + 0
     #.8D1 * t55 - 0.8D1 * t61) + wd * (-0.8D1 * t55 + 0.16D2 * t56 * t5
     #8 * t49 - 0.8D1 * t48 - 0.16D2 * t46 - 0.16D2 * t52 + 0.16D2 * t61
     # - 0.4D1 * t29 * t31 * t73 * t10 * x1 * t5 * t42 * x3)) / t26 / s 
     #/ z / 0.3141592653589793D1 / 0.9D1

      end function
  
   
 

      doubleprecision function rrqg2qgh83J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + x1 * t1
      t5 = 0.1D1 / t4
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t20 = t8 * t4 + x2 * t10 - t19
      t21 = x1 * t5 * t20
      t23 = 0.1D1 - x1
      t26 = s - t2 * t21 - t2 * t23 * x3
      t27 = s ** 2
      t29 = t26 * t27 * s
      t30 = t1 ** 2
      t31 = t30 * t1
      t32 = t29 * t31
      t33 = x1 ** 2
      t35 = t4 ** 2
      t38 = t33 * x1 / t35 / t4
      t42 = t10 * t7 * t4 + x2 * x3 + t19
      t43 = t42 ** 2
      t46 = t32 * t38 * t20 * t43
      t48 = t29 * t1 * t21
      t49 = t20 ** 2
      t52 = t32 * t38 * t49 * t42
      t55 = t32 * t38 * t49 * t20
      t56 = t29 * t30
      t57 = 0.1D1 / t35
      t58 = t33 * t57
      t61 = t56 * t58 * t20 * t42
      t73 = t23 ** 2
      t75 = t29 * t31 * t73
      t76 = t10 * x1
      t77 = t5 * t42
      t81 = 0.4D1 * t75 * t76 * t77 * x3
      t93 = t10 * t33 * t57 * t43
      t96 = t10 ** 2
      t101 = t27 ** 2
      rrqg2qgh83J3 = -(wd * (0.8D1 * t46 + 0.8D1 * t48 + 0.8D1 * t52 + 0
     #.8D1 * t55 - 0.8D1 * t61) + wd * (-0.8D1 * t55 + 0.16D2 * t56 * t5
     #8 * t49 - 0.8D1 * t48 - 0.16D2 * t46 - 0.16D2 * t52 + 0.16D2 * t61
     # - t81) + wd * (-0.4D1 * t29 * t30 * t23 * t76 * t77 - t81 + 0.8D1
     # * t29 * t31 * t23 * t93 + 0.4D1 * t75 * t96 * x1 * t77 - 0.4D1 * 
     #t101 * t31 * t23 * t93 + 0.4D1 * t101 * t30 * x1 * t77 * t23 * t10
     #)) / t26 / s / z / 0.3141592653589793D1 / 0.9D1

      end function
  
   
 

      doubleprecision function rrqg2qgh83J4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + x1 * t1
      t5 = 0.1D1 / t4
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t20 = t8 * t4 + x2 * t10 - t19
      t21 = x1 * t5 * t20
      t23 = 0.1D1 - x1
      t26 = s - t2 * t21 - t2 * t23 * x3
      t27 = s ** 2
      t29 = t26 * t27 * s
      t30 = t1 ** 2
      t31 = t30 * t1
      t32 = t29 * t31
      t33 = x1 ** 2
      t35 = t4 ** 2
      t38 = t33 * x1 / t35 / t4
      t42 = t10 * t7 * t4 + x2 * x3 + t19
      t43 = t42 ** 2
      t46 = t32 * t38 * t20 * t43
      t48 = t29 * t1 * t21
      t49 = t20 ** 2
      t52 = t32 * t38 * t49 * t42
      t55 = t32 * t38 * t49 * t20
      t56 = t29 * t30
      t57 = 0.1D1 / t35
      t58 = t33 * t57
      t61 = t56 * t58 * t20 * t42
      t73 = t23 ** 2
      t75 = t29 * t31 * t73
      t76 = t10 * x1
      t77 = t5 * t42
      t81 = 0.4D1 * t75 * t76 * t77 * x3
      t93 = t10 * t33 * t57 * t43
      t96 = t10 ** 2
      t101 = t27 ** 2
      rrqg2qgh83J4 = -(wd * (0.8D1 * t46 + 0.8D1 * t48 + 0.8D1 * t52 + 0
     #.8D1 * t55 - 0.8D1 * t61) + wd * (-0.8D1 * t55 + 0.16D2 * t56 * t5
     #8 * t49 - 0.8D1 * t48 - 0.16D2 * t46 - 0.16D2 * t52 + 0.16D2 * t61
     # - t81) + wd * (-0.4D1 * t29 * t30 * t23 * t76 * t77 - t81 + 0.8D1
     # * t29 * t31 * t23 * t93 + 0.4D1 * t75 * t96 * x1 * t77 - 0.4D1 * 
     #t101 * t31 * t23 * t93 + 0.4D1 * t101 * t30 * x1 * t77 * t23 * t10
     #)) / t26 / s / z / 0.3141592653589793D1 / 0.9D1

      end function
  
   
 

      doubleprecision function rrqg2qgh83J5
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + x1 * t1
      t5 = 0.1D1 / t4
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t20 = t8 * t4 + x2 * t10 - t19
      t21 = x1 * t5 * t20
      t23 = 0.1D1 - x1
      t26 = s - t2 * t21 - t2 * t23 * x3
      t27 = s ** 2
      t29 = t26 * t27 * s
      t30 = t1 ** 2
      t31 = t30 * t1
      t32 = t29 * t31
      t33 = x1 ** 2
      t35 = t4 ** 2
      t38 = t33 * x1 / t35 / t4
      t42 = t10 * t7 * t4 + x2 * x3 + t19
      t43 = t42 ** 2
      t46 = t32 * t38 * t20 * t43
      t48 = t29 * t1 * t21
      t49 = t20 ** 2
      t52 = t32 * t38 * t49 * t42
      t55 = t32 * t38 * t49 * t20
      t56 = t29 * t30
      t57 = 0.1D1 / t35
      t58 = t33 * t57
      t61 = t56 * t58 * t20 * t42
      t73 = t23 ** 2
      t75 = t29 * t31 * t73
      t76 = t10 * x1
      t77 = t5 * t42
      t81 = 0.4D1 * t75 * t76 * t77 * x3
      t93 = t10 * t33 * t57 * t43
      t96 = t10 ** 2
      t101 = t27 ** 2
      rrqg2qgh83J5 = -(wd * (0.8D1 * t46 + 0.8D1 * t48 + 0.8D1 * t52 + 0
     #.8D1 * t55 - 0.8D1 * t61) + wd * (-0.8D1 * t55 + 0.16D2 * t56 * t5
     #8 * t49 - 0.8D1 * t48 - 0.16D2 * t46 - 0.16D2 * t52 + 0.16D2 * t61
     # - t81) + wd * (-0.4D1 * t29 * t30 * t23 * t76 * t77 - t81 + 0.8D1
     # * t29 * t31 * t23 * t93 + 0.4D1 * t75 * t96 * x1 * t77 - 0.4D1 * 
     #t101 * t31 * t23 * t93 + 0.4D1 * t101 * t30 * x1 * t77 * t23 * t10
     #)) / t26 / s / z / 0.3141592653589793D1 / 0.9D1

      end function
  
   
 

      doubleprecision function rrqg2qgh83J6
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + x1 * t1
      t5 = 0.1D1 / t4
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t20 = t8 * t4 + x2 * t10 - t19
      t21 = x1 * t5 * t20
      t23 = 0.1D1 - x1
      t26 = s - t2 * t21 - t2 * t23 * x3
      t27 = s ** 2
      t29 = t26 * t27 * s
      t30 = t1 ** 2
      t31 = t30 * t1
      t32 = t29 * t31
      t33 = x1 ** 2
      t35 = t4 ** 2
      t38 = t33 * x1 / t35 / t4
      t39 = t20 ** 2
      t44 = t29 * t30
      t45 = 0.1D1 / t35
      t46 = t33 * t45
      t56 = t10 * t7 * t4 + x2 * x3 + t19
      t57 = t56 ** 2
      t70 = t23 ** 2
      t72 = t29 * t31 * t70
      t73 = t10 * x1
      t74 = t5 * t56
      t78 = 0.4D1 * t72 * t73 * t74 * x3
      t90 = t10 * t33 * t45 * t57
      t93 = t10 ** 2
      t98 = t27 ** 2
      rrqg2qgh83J6 = -(wd * (-0.8D1 * t32 * t38 * t39 * t20 + 0.16D2 * t
     #44 * t46 * t39 - 0.8D1 * t29 * t1 * t21 - 0.16D2 * t32 * t38 * t20
     # * t57 - 0.16D2 * t32 * t38 * t39 * t56 + 0.16D2 * t44 * t46 * t20
     # * t56 - t78) + wd * (-0.4D1 * t29 * t30 * t23 * t73 * t74 - t78 +
     # 0.8D1 * t29 * t31 * t23 * t90 + 0.4D1 * t72 * t93 * x1 * t74 - 0.
     #4D1 * t98 * t31 * t23 * t90 + 0.4D1 * t98 * t30 * x1 * t74 * t23 *
     # t10)) / t26 / s / z / 0.3141592653589793D1 / 0.9D1

      end function
  
   
 

      doubleprecision function rrqg2qgh83J7
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + x1 * t1
      t5 = 0.1D1 / t4
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t23 = 0.1D1 - x1
      t26 = s - t2 * x1 * t5 * (t8 * t4 + x2 * t10 - t19) - t2 * t23 * x
     #3
      t27 = s ** 2
      t29 = t26 * t27 * s
      t30 = t1 ** 2
      t33 = t10 * x1
      t37 = t10 * t7 * t4 + x2 * x3 + t19
      t38 = t5 * t37
      t42 = t30 * t1
      t43 = t23 ** 2
      t45 = t29 * t42 * t43
      t52 = x1 ** 2
      t54 = t4 ** 2
      t56 = t37 ** 2
      t58 = t10 * t52 / t54 * t56
      t61 = t10 ** 2
      t66 = t27 ** 2
      rrqg2qgh83J7 = -wd * (-0.4D1 * t29 * t30 * t23 * t33 * t38 - 0.4D1
     # * t45 * t33 * t38 * x3 + 0.8D1 * t29 * t42 * t23 * t58 + 0.4D1 * 
     #t45 * t61 * x1 * t38 - 0.4D1 * t66 * t42 * t23 * t58 + 0.4D1 * t66
     # * t30 * x1 * t38 * t23 * t10) / t26 / s / z / 0.3141592653589793D
     #1 / 0.9D1

      end function
  
   
 

      doubleprecision function rrqg2qgh84J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t5 = t4 ** 2
      t7 = x1 ** 2
      t8 = t7 ** 2
      t9 = t2 * t5 * t8
      t11 = z + x1 * t3
      t12 = t11 ** 2
      t13 = t12 ** 2
      t14 = 0.1D1 / t13
      t15 = 0.1D1 - x2
      t16 = x3 * t15
      t18 = 0.1D1 - x3
      t21 = cos(x4 * 0.3141592653589793D1)
      t25 = Sqrt(t16 * t11 * x2 * t18)
      t27 = 0.2D1 * t21 * t25
      t28 = t16 * t11 + x2 * t18 - t27
      t33 = t18 * t15 * t11 + x2 * x3 + t27
      t34 = t33 ** 2
      t35 = t34 * t33
      t39 = t4 * t3
      t40 = t2 * t39
      t41 = t7 * x1
      t43 = 0.1D1 / t12 / t11
      t44 = t41 * t43
      t45 = t28 ** 2
      t46 = t45 * t28
      t47 = t44 * t46
      t50 = t40 * t41
      t55 = t2 * t4
      t57 = 0.1D1 / t12
      t66 = s * t1
      t70 = s * t3
      t72 = x1 / t11
      t73 = t72 * t33
      t78 = s - t70 * t73 - t70 * (0.1D1 - x1) * t18
      t82 = t78 * t66
      t83 = t82 * t39
      t88 = t82 * t3
      t89 = t72 * t28
      t94 = t82 * t4
      t95 = t7 * t57
      t96 = t95 * t45
      t122 = -0.9D1 * t9 * t14 * t28 * t35 - 0.18D2 * t40 * t47 - 0.36D2
     # * t50 * t43 * t45 * t33 - 0.27D2 * t55 * t7 * t57 * t28 * t33 - 0
     #.27D2 * t50 * t43 * t28 * t34 - 0.36D2 * t66 * t39 * t41 * t43 * t
     #35 * t78 + 0.63D2 * t83 * t44 * t28 * t34 + 0.9D1 * t88 * t89 - 0.
     #36D2 * t88 * t73 + 0.18D2 * t94 * t96 + 0.54D2 * t83 * t44 * t45 *
     # t33 - 0.18D2 * t9 * t14 * t46 * t33 - 0.18D2 * t9 * t14 * t45 * t
     #34 + 0.36D2 * t94 * t95 * t28 * t33 - 0.18D2 * t55 * t96 - 0.9D1 *
     # t2 * t3 * t89 + 0.18D2 * t83 * t47
      rrqg2qgh84J1 = -wd * t122 / t78 / s / z / 0.3141592653589793D1 / 0
     #.9D1

      end function
  
   
 

      doubleprecision function rrqg2qgh84J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t5 = t4 ** 2
      t6 = t2 * t5
      t7 = x1 ** 2
      t8 = t7 ** 2
      t9 = t6 * t8
      t11 = z + x1 * t3
      t12 = t11 ** 2
      t13 = t12 ** 2
      t14 = 0.1D1 / t13
      t15 = 0.1D1 - x2
      t16 = x3 * t15
      t18 = 0.1D1 - x3
      t21 = cos(x4 * 0.3141592653589793D1)
      t25 = Sqrt(t16 * t11 * x2 * t18)
      t27 = 0.2D1 * t21 * t25
      t28 = t16 * t11 + x2 * t18 - t27
      t33 = t18 * t15 * t11 + x2 * x3 + t27
      t34 = t33 ** 2
      t35 = t34 * t33
      t39 = t4 * t3
      t40 = t2 * t39
      t41 = t7 * x1
      t43 = 0.1D1 / t12 / t11
      t44 = t41 * t43
      t45 = t28 ** 2
      t46 = t45 * t28
      t47 = t44 * t46
      t49 = 0.18D2 * t40 * t47
      t50 = t40 * t41
      t55 = t2 * t4
      t57 = 0.1D1 / t12
      t66 = s * t1
      t67 = t66 * t39
      t70 = s * t3
      t71 = 0.1D1 / t11
      t72 = x1 * t71
      t73 = t72 * t33
      t75 = 0.1D1 - x1
      t78 = s - t70 * t73 - t70 * t75 * t18
      t81 = 0.36D2 * t67 * t41 * t43 * t35 * t78
      t82 = t78 * t66
      t83 = t82 * t39
      t88 = t82 * t3
      t89 = t72 * t28
      t93 = 0.36D2 * t88 * t73
      t94 = t82 * t4
      t95 = t7 * t57
      t96 = t95 * t45
      t99 = t33 * t45
      t106 = 0.18D2 * t9 * t14 * t46 * t33
      t111 = t28 * t33
      t121 = 0.18D2 * t83 * t47
      t122 = -0.9D1 * t9 * t14 * t28 * t35 - t49 - 0.36D2 * t50 * t43 * 
     #t45 * t33 - 0.27D2 * t55 * t7 * t57 * t28 * t33 - 0.27D2 * t50 * t
     #43 * t28 * t34 - t81 + 0.63D2 * t83 * t44 * t28 * t34 + 0.9D1 * t8
     #8 * t89 - t93 + 0.18D2 * t94 * t96 + 0.54D2 * t83 * t44 * t99 - t1
     #06 - 0.18D2 * t9 * t14 * t45 * t34 + 0.36D2 * t94 * t95 * t111 - 0
     #.18D2 * t55 * t96 - 0.9D1 * t2 * t3 * t89 + t121
      t127 = t71 * t33
      t133 = t75 * x3
      t134 = t57 * t45 * t133
      t137 = t75 ** 2
      t139 = x3 ** 2
      t141 = t71 * t28
      t150 = t137 * t139
      t177 = t106 + t49 + 0.36D2 * t82 * t4 * t75 * x3 * x1 * t127 - t12
     #1 - 0.18D2 * t40 * t7 * t134 - 0.18D2 * t40 * t137 * t139 * x1 * t
     #141 - 0.18D2 * t6 * t44 * t99 * t133 - 0.18D2 * t6 * t95 * t111 * 
     #t150 + 0.18D2 * t82 * t39 * t7 * t134 + t81 + 0.18D2 * t82 * t39 *
     # x1 * t141 * t150 - 0.72D2 * t94 * t95 * t34 - 0.36D2 * t67 * t133
     # * t78 * t7 * t57 * t34 - 0.36D2 * t67 * t150 * t78 * x1 * t127 + 
     #t93
      rrqg2qgh84J2 = -(wd * t122 + wd * t177) / t78 / s / z / 0.31415926
     #53589793D1 / 0.9D1

      end function
  
   
 

      doubleprecision function rrqg2qgh84J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t5 = t4 ** 2
      t6 = t2 * t5
      t7 = x1 ** 2
      t8 = t7 ** 2
      t9 = t6 * t8
      t11 = z + x1 * t3
      t12 = t11 ** 2
      t13 = t12 ** 2
      t14 = 0.1D1 / t13
      t15 = 0.1D1 - x2
      t16 = x3 * t15
      t18 = 0.1D1 - x3
      t21 = cos(x4 * 0.3141592653589793D1)
      t25 = Sqrt(t16 * t11 * x2 * t18)
      t27 = 0.2D1 * t21 * t25
      t28 = t16 * t11 + x2 * t18 - t27
      t33 = t18 * t15 * t11 + x2 * x3 + t27
      t34 = t33 ** 2
      t35 = t34 * t33
      t39 = t4 * t3
      t40 = t2 * t39
      t41 = t7 * x1
      t43 = 0.1D1 / t12 / t11
      t44 = t41 * t43
      t45 = t28 ** 2
      t46 = t45 * t28
      t47 = t44 * t46
      t49 = 0.18D2 * t40 * t47
      t50 = t40 * t41
      t55 = t2 * t4
      t57 = 0.1D1 / t12
      t66 = s * t1
      t67 = t66 * t39
      t70 = s * t3
      t71 = 0.1D1 / t11
      t72 = x1 * t71
      t73 = t72 * t33
      t75 = 0.1D1 - x1
      t78 = s - t70 * t73 - t70 * t75 * t18
      t81 = 0.36D2 * t67 * t41 * t43 * t35 * t78
      t82 = t78 * t66
      t83 = t82 * t39
      t88 = t82 * t3
      t89 = t72 * t28
      t93 = 0.36D2 * t88 * t73
      t94 = t82 * t4
      t95 = t7 * t57
      t96 = t95 * t45
      t99 = t33 * t45
      t106 = 0.18D2 * t9 * t14 * t46 * t33
      t111 = t28 * t33
      t121 = 0.18D2 * t83 * t47
      t122 = -0.9D1 * t9 * t14 * t28 * t35 - t49 - 0.36D2 * t50 * t43 * 
     #t45 * t33 - 0.27D2 * t55 * t7 * t57 * t28 * t33 - 0.27D2 * t50 * t
     #43 * t28 * t34 - t81 + 0.63D2 * t83 * t44 * t28 * t34 + 0.9D1 * t8
     #8 * t89 - t93 + 0.18D2 * t94 * t96 + 0.54D2 * t83 * t44 * t99 - t1
     #06 - 0.18D2 * t9 * t14 * t45 * t34 + 0.36D2 * t94 * t95 * t111 - 0
     #.18D2 * t55 * t96 - 0.9D1 * t2 * t3 * t89 + t121
      t127 = t71 * t33
      t133 = t75 * x3
      t134 = t57 * t45 * t133
      t137 = t75 ** 2
      t139 = x3 ** 2
      t141 = t71 * t28
      t150 = t137 * t139
      t177 = t106 + t49 + 0.36D2 * t82 * t4 * t75 * x3 * x1 * t127 - t12
     #1 - 0.18D2 * t40 * t7 * t134 - 0.18D2 * t40 * t137 * t139 * x1 * t
     #141 - 0.18D2 * t6 * t44 * t99 * t133 - 0.18D2 * t6 * t95 * t111 * 
     #t150 + 0.18D2 * t82 * t39 * t7 * t134 + t81 + 0.18D2 * t82 * t39 *
     # x1 * t141 * t150 - 0.72D2 * t94 * t95 * t34 - 0.36D2 * t67 * t133
     # * t78 * t7 * t57 * t34 - 0.36D2 * t67 * t150 * t78 * x1 * t127 + 
     #t93
      rrqg2qgh84J3 = -(wd * t122 + wd * t177) / t78 / s / z / 0.31415926
     #53589793D1 / 0.9D1

      end function
  
   
 

      doubleprecision function rrqg2qgh84J4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t5 = t4 ** 2
      t6 = t2 * t5
      t7 = x1 ** 2
      t8 = t7 ** 2
      t9 = t6 * t8
      t11 = z + x1 * t3
      t12 = t11 ** 2
      t13 = t12 ** 2
      t14 = 0.1D1 / t13
      t15 = 0.1D1 - x2
      t16 = x3 * t15
      t18 = 0.1D1 - x3
      t21 = cos(x4 * 0.3141592653589793D1)
      t25 = Sqrt(t16 * t11 * x2 * t18)
      t27 = 0.2D1 * t21 * t25
      t28 = t16 * t11 + x2 * t18 - t27
      t33 = t18 * t15 * t11 + x2 * x3 + t27
      t34 = t33 ** 2
      t35 = t34 * t33
      t39 = t4 * t3
      t40 = t2 * t39
      t41 = t7 * x1
      t43 = 0.1D1 / t12 / t11
      t44 = t41 * t43
      t45 = t28 ** 2
      t46 = t45 * t28
      t47 = t44 * t46
      t49 = 0.18D2 * t40 * t47
      t50 = t40 * t41
      t55 = t2 * t4
      t57 = 0.1D1 / t12
      t66 = s * t1
      t67 = t66 * t39
      t70 = s * t3
      t71 = 0.1D1 / t11
      t72 = x1 * t71
      t73 = t72 * t33
      t75 = 0.1D1 - x1
      t78 = s - t70 * t73 - t70 * t75 * t18
      t81 = 0.36D2 * t67 * t41 * t43 * t35 * t78
      t82 = t78 * t66
      t83 = t82 * t39
      t88 = t82 * t3
      t89 = t72 * t28
      t93 = 0.36D2 * t88 * t73
      t94 = t82 * t4
      t95 = t7 * t57
      t96 = t95 * t45
      t99 = t33 * t45
      t106 = 0.18D2 * t9 * t14 * t46 * t33
      t111 = t28 * t33
      t121 = 0.18D2 * t83 * t47
      t122 = -0.9D1 * t9 * t14 * t28 * t35 - t49 - 0.36D2 * t50 * t43 * 
     #t45 * t33 - 0.27D2 * t55 * t7 * t57 * t28 * t33 - 0.27D2 * t50 * t
     #43 * t28 * t34 - t81 + 0.63D2 * t83 * t44 * t28 * t34 + 0.9D1 * t8
     #8 * t89 - t93 + 0.18D2 * t94 * t96 + 0.54D2 * t83 * t44 * t99 - t1
     #06 - 0.18D2 * t9 * t14 * t45 * t34 + 0.36D2 * t94 * t95 * t111 - 0
     #.18D2 * t55 * t96 - 0.9D1 * t2 * t3 * t89 + t121
      t127 = t71 * t33
      t133 = t75 * x3
      t134 = t57 * t45 * t133
      t137 = t75 ** 2
      t139 = x3 ** 2
      t141 = t71 * t28
      t150 = t137 * t139
      t177 = t106 + t49 + 0.36D2 * t82 * t4 * t75 * x3 * x1 * t127 - t12
     #1 - 0.18D2 * t40 * t7 * t134 - 0.18D2 * t40 * t137 * t139 * x1 * t
     #141 - 0.18D2 * t6 * t44 * t99 * t133 - 0.18D2 * t6 * t95 * t111 * 
     #t150 + 0.18D2 * t82 * t39 * t7 * t134 + t81 + 0.18D2 * t82 * t39 *
     # x1 * t141 * t150 - 0.72D2 * t94 * t95 * t34 - 0.36D2 * t67 * t133
     # * t78 * t7 * t57 * t34 - 0.36D2 * t67 * t150 * t78 * x1 * t127 + 
     #t93
      rrqg2qgh84J4 = -(wd * t122 + wd * t177) / t78 / s / z / 0.31415926
     #53589793D1 / 0.9D1

      end function
  
   
 

      doubleprecision function rrqg2qgh84J5
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t5 = t4 ** 2
      t6 = t2 * t5
      t7 = x1 ** 2
      t8 = t7 ** 2
      t9 = t6 * t8
      t11 = z + x1 * t3
      t12 = t11 ** 2
      t13 = t12 ** 2
      t14 = 0.1D1 / t13
      t15 = 0.1D1 - x2
      t16 = x3 * t15
      t18 = 0.1D1 - x3
      t21 = cos(x4 * 0.3141592653589793D1)
      t25 = Sqrt(t16 * t11 * x2 * t18)
      t27 = 0.2D1 * t21 * t25
      t28 = t16 * t11 + x2 * t18 - t27
      t33 = t18 * t15 * t11 + x2 * x3 + t27
      t34 = t33 ** 2
      t35 = t34 * t33
      t39 = t4 * t3
      t40 = t2 * t39
      t41 = t7 * x1
      t43 = 0.1D1 / t12 / t11
      t44 = t41 * t43
      t45 = t28 ** 2
      t46 = t45 * t28
      t47 = t44 * t46
      t49 = 0.18D2 * t40 * t47
      t50 = t40 * t41
      t55 = t2 * t4
      t57 = 0.1D1 / t12
      t66 = s * t1
      t67 = t66 * t39
      t70 = s * t3
      t71 = 0.1D1 / t11
      t72 = x1 * t71
      t73 = t72 * t33
      t75 = 0.1D1 - x1
      t78 = s - t70 * t73 - t70 * t75 * t18
      t81 = 0.36D2 * t67 * t41 * t43 * t35 * t78
      t82 = t78 * t66
      t83 = t82 * t39
      t88 = t82 * t3
      t89 = t72 * t28
      t93 = 0.36D2 * t88 * t73
      t94 = t82 * t4
      t95 = t7 * t57
      t96 = t95 * t45
      t99 = t33 * t45
      t106 = 0.18D2 * t9 * t14 * t46 * t33
      t111 = t28 * t33
      t121 = 0.18D2 * t83 * t47
      t122 = -0.9D1 * t9 * t14 * t28 * t35 - t49 - 0.36D2 * t50 * t43 * 
     #t45 * t33 - 0.27D2 * t55 * t7 * t57 * t28 * t33 - 0.27D2 * t50 * t
     #43 * t28 * t34 - t81 + 0.63D2 * t83 * t44 * t28 * t34 + 0.9D1 * t8
     #8 * t89 - t93 + 0.18D2 * t94 * t96 + 0.54D2 * t83 * t44 * t99 - t1
     #06 - 0.18D2 * t9 * t14 * t45 * t34 + 0.36D2 * t94 * t95 * t111 - 0
     #.18D2 * t55 * t96 - 0.9D1 * t2 * t3 * t89 + t121
      t127 = t71 * t33
      t133 = t75 * x3
      t134 = t57 * t45 * t133
      t137 = t75 ** 2
      t139 = x3 ** 2
      t141 = t71 * t28
      t150 = t137 * t139
      t177 = t106 + t49 + 0.36D2 * t82 * t4 * t75 * x3 * x1 * t127 - t12
     #1 - 0.18D2 * t40 * t7 * t134 - 0.18D2 * t40 * t137 * t139 * x1 * t
     #141 - 0.18D2 * t6 * t44 * t99 * t133 - 0.18D2 * t6 * t95 * t111 * 
     #t150 + 0.18D2 * t82 * t39 * t7 * t134 + t81 + 0.18D2 * t82 * t39 *
     # x1 * t141 * t150 - 0.72D2 * t94 * t95 * t34 - 0.36D2 * t67 * t133
     # * t78 * t7 * t57 * t34 - 0.36D2 * t67 * t150 * t78 * x1 * t127 + 
     #t93
      rrqg2qgh84J5 = -(wd * t122 + wd * t177) / t78 / s / z / 0.31415926
     #53589793D1 / 0.9D1

      end function
  
   
 

      doubleprecision function rrqg2qgh84J6
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t5 = t4 ** 2
      t6 = t2 * t5
      t7 = x1 ** 2
      t8 = t7 ** 2
      t11 = z + x1 * t3
      t12 = t11 ** 2
      t13 = t12 ** 2
      t15 = 0.1D1 - x2
      t16 = x3 * t15
      t18 = 0.1D1 - x3
      t21 = cos(x4 * 0.3141592653589793D1)
      t25 = Sqrt(t16 * t11 * x2 * t18)
      t27 = 0.2D1 * t21 * t25
      t28 = t16 * t11 + x2 * t18 - t27
      t29 = t28 ** 2
      t30 = t29 * t28
      t35 = t18 * t15 * t11 + x2 * x3 + t27
      t39 = t4 * t3
      t40 = t2 * t39
      t41 = t7 * x1
      t43 = 0.1D1 / t12 / t11
      t44 = t41 * t43
      t45 = t44 * t30
      t48 = s * t3
      t49 = 0.1D1 / t11
      t51 = x1 * t49 * t35
      t53 = 0.1D1 - x1
      t56 = s - t48 * t51 - t48 * t53 * t18
      t57 = s * t1
      t58 = t56 * t57
      t62 = t49 * t35
      t70 = 0.1D1 / t12
      t72 = t53 * x3
      t73 = t70 * t29 * t72
      t76 = t53 ** 2
      t78 = x3 ** 2
      t80 = t49 * t28
      t89 = t7 * t70
      t92 = t76 * t78
      t100 = t57 * t39
      t102 = t35 ** 2
      t131 = 0.18D2 * t6 * t8 / t13 * t30 * t35 + 0.18D2 * t40 * t45 + 0
     #.36D2 * t58 * t4 * t53 * x3 * x1 * t62 - 0.18D2 * t58 * t39 * t45 
     #- 0.18D2 * t40 * t7 * t73 - 0.18D2 * t40 * t76 * t78 * x1 * t80 - 
     #0.18D2 * t6 * t44 * t29 * t35 * t72 - 0.18D2 * t6 * t89 * t28 * t3
     #5 * t92 + 0.18D2 * t58 * t39 * t7 * t73 + 0.36D2 * t100 * t41 * t4
     #3 * t102 * t35 * t56 + 0.18D2 * t58 * t39 * x1 * t80 * t92 - 0.72D
     #2 * t58 * t4 * t89 * t102 - 0.36D2 * t100 * t72 * t56 * t7 * t70 *
     # t102 - 0.36D2 * t100 * t92 * t56 * x1 * t62 + 0.36D2 * t58 * t3 *
     # t51
      rrqg2qgh84J6 = -wd * t131 / t56 / s / z / 0.3141592653589793D1 / 0
     #.9D1

      end function
  
 