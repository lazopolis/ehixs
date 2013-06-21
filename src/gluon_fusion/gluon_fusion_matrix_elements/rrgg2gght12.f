  
      subroutine rrgg2gght12
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2gght12s1e1  
      doubleprecision rrgg2gght12s1e0  
      doubleprecision rrgg2gght12s1em1  
      doubleprecision rrgg2gght12s1em2  
      doubleprecision rrgg2gght12s1em3  
      doubleprecision rrgg2gght12s1em4  
      doubleprecision rrgg2gght12s2e1  
      doubleprecision rrgg2gght12s2e0  
      doubleprecision rrgg2gght12s2em1  
      doubleprecision rrgg2gght12s2em2  
      doubleprecision rrgg2gght12s2em3  
      doubleprecision rrgg2gght12s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght12s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght12s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gght12s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght12s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght12s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght12s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gght12s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght12s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gght12s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght12s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gght12s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght12s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gght12s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t6 = z * wd
      t7 = x2 * x3
      t8 = x4 * 0.3141592653589793D1
      t9 = Sin(t8)
      t10 = t9 ** 2
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t13 = t10 * t12
      t14 = t13 * t4
      t17 = log(-0.4D1 * t7 * t14)
      t18 = t17 ** 2
      t19 = x3 * t4
      t20 = Sqrt(-t19)
      t21 = t20 ** 2
      t23 = cos(t8)
      t24 = t23 ** 2
      t28 = lh * z
      t29 = t28 * wd
      t34 = lh ** 2
      t36 = 0.3141592653589793D1 ** 2
      t38 = -0.180D3 * t34 + 0.30D2 * t36
      t39 = t38 * z
      t41 = wd * t21 * t24
      t42 = t39 * t41
      t45 = 0.1D1 / x2
      t52 = log(-0.4D1 * x3 * t10 * t12 * t4)
      t53 = t52 ** 2
      t65 = z * t24
      t66 = t65 * wd
      t69 = x1 ** 2
      t73 = log(-0.4D1 * t7 * t69 * t14)
      t81 = 0.1D1 / x1
      t85 = x3 * t69
      t88 = log(-0.4D1 * t85 * t14)
      t89 = t88 ** 2
      t94 = t28 * t24
      t102 = (0.720D3 * t6 * t18 * t21 * t24 + 0.2880D4 * t29 * t17 * t2
     #1 * t24 - 0.16D2 * t42) * t45 / 0.320D3 - (0.90D2 * t53 * lh - 0.6
     #0D2 * lh * t36 + 0.2884936567583026D3 + 0.120D3 * t34 * lh + 0.15D
     #2 * t53 * t52 - t52 * t38) * t21 * t66 / 0.20D2 + (-0.1440D4 * t6 
     #* t73 * t21 * t24 - 0.2880D4 * t28 * t41) * t81 * t45 / 0.160D3 - 
     #(-0.45D2 * t65 * wd * t89 * t21 - 0.180D3 * t94 * wd * t88 * t21 +
     # t42) * t81 / 0.10D2
      t103 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t
     #102)
      t105 = -0.1D1 + x1
      t106 = x3 * x1
      t107 = t106 * z
      t108 = 0.2D1 * t7
      t109 = t106 * x2
      t110 = x2 * z
      t111 = t106 * t110
      t112 = sqrt(x2)
      t113 = t23 * t112
      t114 = -0.1D1 + t112
      t115 = x3 * t114
      t116 = t112 + 0.1D1
      t117 = x1 * z
      t118 = 0.1D1 - x1 + t117
      t122 = Sqrt(t115 * t116 * t118 * t4)
      t124 = 0.2D1 * t113 * t122
      t127 = 0.1D1 / t118
      t130 = t2 * t106
      t131 = x1 * x2
      t132 = t131 * z
      t133 = 0.1D1 - x1 + t117 - x2 + t131 - t132 - x3 + t106 - t107 + t
     #108 - t109 + t111 + t124
      t137 = t4 * s
      t139 = t137 * t1 * x1
      t140 = t1 ** 2
      t146 = t10 * t69
      t147 = t105 ** 2
      t148 = t12 * t147
      t156 = log(0.4D1 * t146 * t148 * t127 * t114 * t116 * t7 * t4)
      t160 = t112 * x2
      t161 = t160 * z
      t163 = 0.4D1 * t161 * x3
      t164 = 0.2D1 * t160
      t165 = t23 * t122
      t166 = t165 * x1
      t170 = x2 * t23
      t173 = x3 * t112
      t175 = 0.2D1 * t173 * z
      t178 = x1 * t112
      t181 = t69 * t112
      t184 = t69 * t11
      t188 = t160 * t69
      t191 = t160 * x1
      t198 = 0.3D1 * t112
      t201 = -0.4D1 * t85 * t112 - t163 - t164 + 0.4D1 * t166 - 0.4D1 * 
     #t161 * x1 + 0.4D1 * t170 * t122 + t175 + 0.10D2 * t106 * t112 + 0.
     #6D1 * t178 * z - 0.4D1 * t181 * z + 0.2D1 * t184 * t112 - t178 * t
     #11 + 0.2D1 * t188 * x3 - 0.6D1 * t191 * x3 + 0.2D1 * t188 * z + t1
     #91 * t11 - t188 * t11 + t198 + 0.4D1 * t110 * t166
      t203 = t112 * z
      t206 = 0.6D1 * t173
      t208 = 0.4D1 * t160 * x3
      t209 = 0.2D1 * t161
      t221 = t11 * x3
      t229 = t122 * x1
      t238 = 0.3D1 * t191 - t188 - t203 - 0.5D1 * t178 + 0.2D1 * t181 - 
     #t206 + t208 + t209 - 0.4D1 * t165 - 0.4D1 * t184 * t173 + 0.8D1 * 
     #t85 * t203 + 0.2D1 * t106 * t112 * t11 - 0.4D1 * t188 * z * x3 - 0
     #.2D1 * t191 * t221 + 0.2D1 * t188 * t221 - 0.12D2 * t106 * t203 - 
     #0.4D1 * z * t23 * t229 - 0.4D1 * t170 * t229 + 0.8D1 * t161 * t106
     # - 0.4D1 * t110 * t165
      t240 = (t201 + t238) ** 2
      t242 = (-0.1D1 + x1 - t117 + x2 - t131 - t110 + t132) ** 2
      t245 = t240 / t242 * t127
      t250 = -0.90D2 * t6 * t156 * t245 - 0.180D3 * t29 * t245
      t254 = FJET(XB1, XB2, s, t2 * t105 * (-x3 + t106 - t107 + t108 - t
     #109 + t111 - x2 + t124) * t127, t130, -t2 * t105 * t133 * t127, -t
     #139, -s * t140 * x2 * x1 * t105 * t127, t250 * t81 * t45 / 0.160D3
     #)
      t261 = Sqrt(t115 * t116 * t4)
      t263 = 0.2D1 * t113 * t261
      t270 = t116 * x2 * t19
      t273 = log(0.4D1 * t13 * t114 * t270)
      t274 = t273 ** 2
      t277 = t23 * t261
      t281 = -t175 + t163 - 0.4D1 * t170 * t261 + t164 + t203 + t206 - t
     #198 + 0.4D1 * t110 * t277 + 0.4D1 * t277 - t208 - t209
      t282 = t281 ** 2
      t285 = (0.1D1 - x2 + t110) ** 2
      t286 = 0.1D1 / t285
      t295 = wd * t282 * t286
      t304 = log(0.4D1 * t146 * t12 * t114 * t270)
      t315 = (-0.45D2 * t6 * t274 * t282 * t286 - 0.180D3 * t29 * t273 *
     # t282 * t286 + t39 * t295) * t45 / 0.320D3 + (0.90D2 * t6 * t304 *
     # t282 * t286 + 0.180D3 * t28 * t295) * t81 * t45 / 0.160D3
      t316 = FJET(XB1, XB2, s, -t2 * (-x3 + t108 - x2 + t263), 0.0D0, t2
     # * (0.1D1 - x2 - x3 + t108 + t263), 0.0D0, 0.0D0, t315)
      t319 = t1 * t105
      t328 = log(-0.4D1 * t127 * t10 * t148 * t85 * x2 * t4)
      t332 = Sqrt(-x3 * t118 * t4)
      t333 = t332 ** 2
      t335 = t24 * t333 * t127
      t350 = log(-0.4D1 * t85 * t10 * t12 * t127 * t147 * t4)
      t351 = t350 ** 2
      t368 = (0.1440D4 * t6 * t328 * t335 + 0.2880D4 * t29 * t335) * t81
     # * t45 / 0.160D3 - (0.45D2 * t66 * t351 * t333 * t127 + 0.180D3 * 
     #t94 * wd * t350 * t333 * t127 - t39 * t24 * wd * t333 * t127) * t8
     #1 / 0.10D2
      t369 = FJET(XB1, XB2, s, -x3 * s * t319, t130, t137 * t319, -t139,
     # 0.0D0, t368)
      rrgg2gght12s1e1 = t103 * t102 + t254 * t250 * t81 * t45 / 0.160D3 
     #+ t316 * t315 + t369 * t368

      end function



      doubleprecision function rrgg2gght12s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t6 = z * wd
      t7 = x2 * x3
      t8 = x4 * 0.3141592653589793D1
      t9 = Sin(t8)
      t10 = t9 ** 2
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t13 = t10 * t12
      t14 = t13 * t4
      t17 = log(-0.4D1 * t7 * t14)
      t18 = x3 * t4
      t19 = Sqrt(-t18)
      t20 = t19 ** 2
      t22 = cos(t8)
      t23 = t22 ** 2
      t27 = lh * z
      t30 = t27 * wd * t20 * t23
      t33 = 0.1D1 / x2
      t37 = 0.1D1 / x1
      t42 = z * t23
      t43 = x1 ** 2
      t44 = x3 * t43
      t47 = log(-0.4D1 * t44 * t14)
      t60 = log(-0.4D1 * x3 * t10 * t12 * t4)
      t63 = t60 ** 2
      t65 = lh ** 2
      t67 = 0.3141592653589793D1 ** 2
      t71 = t42 * wd
      t74 = (-0.1440D4 * t6 * t17 * t20 * t23 - 0.2880D4 * t30) * t33 / 
     #0.320D3 + 0.9D1 * t6 * t20 * t23 * t37 * t33 - (0.90D2 * t42 * wd 
     #* t47 * t20 + 0.180D3 * t30) * t37 / 0.10D2 - (-0.180D3 * t60 * lh
     # - 0.45D2 * t63 - 0.180D3 * t65 + 0.30D2 * t67) * t20 * t71 / 0.20
     #D2
      t75 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t7
     #4)
      t77 = -0.1D1 + x1
      t78 = x3 * x1
      t79 = t78 * z
      t80 = 0.2D1 * t7
      t81 = t78 * x2
      t82 = x2 * z
      t83 = t78 * t82
      t84 = sqrt(x2)
      t85 = t22 * t84
      t86 = -0.1D1 + t84
      t87 = x3 * t86
      t88 = t84 + 0.1D1
      t89 = x1 * z
      t90 = 0.1D1 - x1 + t89
      t94 = Sqrt(t87 * t88 * t90 * t4)
      t96 = 0.2D1 * t85 * t94
      t99 = 0.1D1 / t90
      t102 = t2 * t78
      t103 = x1 * x2
      t104 = t103 * z
      t105 = 0.1D1 - x1 + t89 - x2 + t103 - t104 - x3 + t78 - t79 + t80 
     #- t81 + t83 + t96
      t109 = t4 * s
      t111 = t109 * t1 * x1
      t112 = t1 ** 2
      t118 = t22 * t94
      t119 = t118 * x1
      t122 = t43 * t11
      t123 = t84 * x3
      t126 = t84 * z
      t132 = t84 * x2
      t133 = t132 * t43
      t137 = t132 * x1
      t138 = t11 * x3
      t146 = t94 * x1
      t149 = x2 * t22
      t152 = t132 * z
      t157 = 0.2D1 * t132
      t158 = x1 * t84
      t160 = t43 * t84
      t162 = 0.6D1 * t123
      t164 = 0.4D1 * t132 * x3
      t165 = 0.2D1 * t152
      t167 = 0.4D1 * t82 * t119 - 0.4D1 * t122 * t123 + 0.8D1 * t44 * t1
     #26 + 0.2D1 * t78 * t84 * t11 - 0.4D1 * t133 * z * x3 - 0.2D1 * t13
     #7 * t138 + 0.2D1 * t133 * t138 - 0.12D2 * t78 * t126 - 0.4D1 * z *
     # t22 * t146 - 0.4D1 * t149 * t146 + 0.8D1 * t152 * t78 - 0.4D1 * t
     #82 * t118 - t157 - 0.5D1 * t158 + 0.2D1 * t160 - t162 + t164 + t16
     #5 - 0.4D1 * t118
      t169 = 0.3D1 * t84
      t176 = 0.2D1 * t123 * z
      t197 = 0.4D1 * t152 * x3
      t198 = 0.3D1 * t137 - t133 - t126 + t169 + 0.4D1 * t119 - 0.4D1 * 
     #t152 * x1 + 0.4D1 * t149 * t94 + t176 + 0.10D2 * t78 * t84 + 0.6D1
     # * t158 * z - 0.4D1 * t160 * z + 0.2D1 * t122 * t84 - t158 * t11 +
     # 0.2D1 * t133 * x3 - 0.6D1 * t137 * x3 + 0.2D1 * t133 * z + t137 *
     # t11 - t133 * t11 - 0.4D1 * t44 * t84 - t197
      t200 = (t167 + t198) ** 2
      t203 = (-0.1D1 + x1 - t89 + x2 - t103 - t82 + t104) ** 2
      t206 = t37 * t33
      t207 = 0.1D1 / t203 * t99 * t206
      t210 = FJET(XB1, XB2, s, t2 * t77 * (-x3 + t78 - t79 + t80 - t81 +
     # t83 - x2 + t96) * t99, t102, -t2 * t77 * t105 * t99, -t111, -s * 
     #t112 * x2 * x1 * t77 * t99, 0.9D1 / 0.16D2 * t6 * t200 * t207)
      t218 = Sqrt(t87 * t88 * t4)
      t220 = 0.2D1 * t85 * t218
      t230 = log(0.4D1 * t13 * t86 * t88 * x2 * t18)
      t233 = t22 * t218
      t237 = -t176 + t197 - 0.4D1 * t149 * t218 + t157 + t126 + t162 - t
     #169 + 0.4D1 * t82 * t233 + 0.4D1 * t233 - t164 - t165
      t238 = t237 ** 2
      t241 = (0.1D1 - x2 + t82) ** 2
      t242 = 0.1D1 / t241
      t258 = (0.90D2 * t6 * t230 * t238 * t242 + 0.180D3 * t27 * wd * t2
     #38 * t242) * t33 / 0.320D3 - 0.9D1 / 0.16D2 * t6 * t238 * t242 * t
     #37 * t33
      t259 = FJET(XB1, XB2, s, -t2 * (-x3 + t80 - x2 + t220), 0.0D0, t2 
     #* (0.1D1 - x2 - x3 + t80 + t220), 0.0D0, 0.0D0, t258)
      t262 = t1 * t77
      t267 = Sqrt(-x3 * t90 * t4)
      t268 = t267 ** 2
      t275 = t77 ** 2
      t280 = log(-0.4D1 * t44 * t10 * t12 * t99 * t275 * t4)
      t293 = -0.9D1 * t71 * t268 * t99 * t206 - (-0.90D2 * t71 * t280 * 
     #t268 * t99 - 0.180D3 * t27 * wd * t23 * t268 * t99) * t37 / 0.10D2
      t294 = FJET(XB1, XB2, s, -x3 * s * t262, t102, t109 * t262, -t111,
     # 0.0D0, t293)
      rrgg2gght12s1e0 = t75 * t74 + 0.9D1 / 0.16D2 * t210 * z * wd * t20
     #0 * t207 + t259 * t258 + t294 * t293

      end function



      doubleprecision function rrgg2gght12s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t6 = z * wd
      t8 = Sqrt(-x3 * t4)
      t9 = t8 ** 2
      t10 = x4 * 0.3141592653589793D1
      t11 = cos(t10)
      t12 = t11 ** 2
      t13 = t9 * t12
      t14 = 0.1D1 / x1
      t19 = Sin(t10)
      t20 = t19 ** 2
      t22 = z ** 2
      t27 = log(-0.4D1 * x3 * t20 / t22 * t4)
      t32 = z * t12 * wd
      t35 = 0.1D1 / x2
      t39 = 0.9D1 * t6 * t13 * t14 - (0.180D3 * lh + 0.90D2 * t27) * t9 
     #* t32 / 0.20D2 + 0.9D1 / 0.2D1 * t6 * t13 * t35
      t40 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t3
     #9)
      t44 = t1 * (-0.1D1 + x1)
      t48 = t4 * s
      t53 = 0.1D1 - x1 + x1 * z
      t56 = Sqrt(-x3 * t53 * t4)
      t57 = t56 ** 2
      t58 = 0.1D1 / t53
      t63 = FJET(XB1, XB2, s, -x3 * s * t44, t2 * x1 * x3, t48 * t44, -t
     #48 * t1 * x1, 0.0D0, -0.9D1 * t32 * t57 * t58 * t14)
      t72 = 0.2D1 * x2 * x3
      t73 = sqrt(x2)
      t80 = Sqrt(x3 * (-0.1D1 + t73) * (t73 + 0.1D1) * t4)
      t82 = 0.2D1 * t11 * t73 * t80
      t87 = x3 * t73
      t90 = t73 * x2
      t91 = t90 * z
      t101 = x2 * z
      t102 = t11 * t80
      t109 = -0.2D1 * t87 * z + 0.4D1 * t91 * x3 - 0.4D1 * x2 * t11 * t8
     #0 + 0.2D1 * t90 + t73 * z + 0.6D1 * t87 - 0.3D1 * t73 + 0.4D1 * t1
     #01 * t102 + 0.4D1 * t102 - 0.4D1 * t90 * x3 - 0.2D1 * t91
      t110 = t109 ** 2
      t112 = (0.1D1 - x2 + t101) ** 2
      t115 = t110 / t112 * t35
      t118 = FJET(XB1, XB2, s, -t2 * (-x3 + t72 - x2 + t82), 0.0D0, t2 *
     # (0.1D1 - x2 - x3 + t72 + t82), 0.0D0, 0.0D0, -0.9D1 / 0.32D2 * t6
     # * t115)
      rrgg2gght12s1em1 = t40 * t39 - 0.9D1 * t63 * z * wd * t12 * t57 * 
     #t58 * t14 - 0.9D1 / 0.32D2 * t118 * z * wd * t115

      end function



      doubleprecision function rrgg2gght12s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t2 = s * (-0.1D1 + z)
      t4 = -0.1D1 + x3
      t8 = Sqrt(-x3 * t4)
      t9 = t8 ** 2
      t11 = cos(x4 * 0.3141592653589793D1)
      t12 = t11 ** 2
      t16 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, 0.
     #9D1 / 0.2D1 * z * wd * t9 * t12)
      rrgg2gght12s1em2 = 0.9D1 / 0.2D1 * t16 * z * wd * t9 * t12

      end function



      doubleprecision function rrgg2gght12s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgg2gght12s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2gght12s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgg2gght12s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2gght12s2e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t6 = x4 * 0.3141592653589793D1
      t7 = Sin(t6)
      t8 = t7 ** 2
      t9 = z ** 2
      t11 = 0.1D1 / t9 / z
      t12 = t8 * t11
      t13 = sqrt(x2)
      t14 = -0.1D1 + t13
      t16 = t13 + 0.1D1
      t19 = t16 * x2 * x3 * t4
      t22 = log(0.4D1 * t12 * t14 * t19)
      t23 = t22 ** 2
      t24 = x3 * t13
      t26 = 0.2D1 * z * t24
      t27 = t13 * z
      t29 = cos(t6)
      t30 = x3 * t14
      t34 = Sqrt(t30 * t16 * z * t4)
      t38 = (t26 - t27 + 0.2D1 * t24 - t13 + 0.4D1 * t29 * t34) ** 2
      t41 = x2 * x3
      t42 = t12 * t4
      t45 = log(-0.4D1 * t41 * t42)
      t46 = t45 ** 2
      t47 = x3 * z
      t49 = Sqrt(-t47 * t4)
      t50 = t49 ** 2
      t52 = t29 ** 2
      t58 = lh * wd
      t66 = lh ** 2
      t68 = 0.3141592653589793D1 ** 2
      t70 = -0.180D3 * t66 + 0.30D2 * t68
      t74 = -t38 + 0.16D2 * t50 * t52
      t77 = 0.1D1 / x2
      t84 = log(-0.4D1 * x3 * t11 * t8 * t4)
      t85 = t84 ** 2
      t104 = x1 ** 2
      t108 = log(-0.4D1 * t41 * t104 * t42)
      t117 = log(0.4D1 * t8 * t104 * t11 * t14 * t19)
      t126 = 0.1D1 / x1
      t130 = t52 * wd
      t131 = x3 * t104
      t134 = log(-0.4D1 * t131 * t42)
      t135 = t134 ** 2
      t139 = lh * t52
      t144 = t70 * t52
      t151 = -(-0.90D2 * wd * (-t23 * t38 / 0.2D1 + 0.8D1 * t46 * t50 * 
     #t52) + 0.180D3 * t58 * (t22 * t38 - 0.16D2 * t45 * t50 * t52) + t7
     #0 * wd * t74) * t77 / 0.320D3 - (0.90D2 * t85 * t50 * lh + t50 * (
     #-0.60D2 * lh * t68 + 0.2884936567583026D3 + 0.120D3 * t66 * lh) + 
     #0.15D2 * t85 * t84 * t50 - t84 * t50 * t70) * t52 * wd / 0.20D2 + 
     #(-0.90D2 * wd * (0.16D2 * t108 * t50 * t52 - t117 * t38) - 0.180D3
     # * t58 * t74) * t126 * t77 / 0.160D3 + (0.720D3 * t130 * t135 * t5
     #0 + 0.2880D4 * t139 * wd * t134 * t50 - 0.16D2 * t144 * wd * t50) 
     #* t126 / 0.160D3
      t152 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t
     #151)
      t154 = x3 * x1
      t157 = -0.1D1 + x1
      t158 = t1 * t157
      t159 = x3 * s * t158
      t160 = t4 * s
      t163 = t160 * t158
      t164 = x1 * z
      t165 = -z - x1 + t164
      t166 = 0.1D1 / t165
      t167 = t166 * t8
      t168 = 0.1D1 / t9
      t169 = t157 ** 2
      t176 = log(0.4D1 * t167 * t168 * t169 * t131 * x2 * t4)
      t182 = Sqrt(x3 * t165 * t4)
      t183 = t182 ** 2
      t184 = z * t52 * t183
      t187 = t58 * t166
      t199 = log(0.4D1 * t131 * t8 * t168 * t166 * t169 * t4)
      t200 = t199 ** 2
      t203 = t166 * z * t183
      t218 = (-0.1440D4 * wd * t176 * t166 * t184 - 0.2880D4 * t187 * t1
     #84) * t126 * t77 / 0.160D3 + (0.720D3 * t130 * t200 * t203 + 0.288
     #0D4 * t139 * wd * t199 * t166 * z * t183 - 0.16D2 * t144 * wd * t2
     #03) * t126 / 0.160D3
      t219 = FJET(XB1, XB2, s, t2 * t154, -t159, -t160 * t1 * x1, t163, 
     #0.0D0, t218)
      t221 = t154 * z
      t222 = t41 * z
      t223 = t154 * x2
      t224 = x2 * z
      t225 = t154 * t224
      t230 = Sqrt(-t30 * t16 * t165 * t4)
      t232 = 0.2D1 * t29 * t13 * t230
      t237 = x1 * x2
      t238 = t237 * z
      t239 = z + x1 - t164 - t224 - t237 + t238 - t47 - t154 + t221 + t2
     #22 + t223 - t225 + t41 + t232
      t243 = t1 ** 2
      t257 = log(-0.4D1 * t167 * t104 * t169 * t168 * t14 * t16 * t41 * 
     #t4)
      t261 = (z + x1 - t164 - t237 + t238) ** 2
      t264 = t29 * t230
      t269 = t13 * x2
      t270 = t269 * x1
      t271 = t9 * x3
      t274 = t269 * t104
      t281 = t13 * t9
      t286 = t104 * t9
      t290 = z * t29 * t230
      t293 = x1 * t13
      t294 = t26 - 0.4D1 * t164 * t264 - 0.4D1 * t237 * t264 + 0.2D1 * t
     #270 * t271 - 0.2D1 * t274 * t271 + 0.4D1 * t154 * t27 - 0.8D1 * t1
     #31 * t27 - 0.6D1 * t154 * t281 + 0.4D1 * t274 * t47 + 0.4D1 * t286
     # * t24 + 0.4D1 * t237 * t290 + t270 - t281 + t274 - t293
      t295 = t104 * t13
      t323 = -0.2D1 * t295 - t27 + 0.2D1 * t154 * t13 - 0.2D1 * t293 * z
     # + 0.4D1 * t295 * z - 0.2D1 * t286 * t13 + 0.3D1 * t293 * t9 + 0.4
     #D1 * x1 * t29 * t230 + 0.2D1 * t281 * x3 - 0.2D1 * t274 * x3 - 0.2
     #D1 * t270 * x3 - 0.2D1 * t274 * z - t270 * t9 + t274 * t9 + 0.4D1 
     #* t290 + 0.4D1 * t131 * t13
      t325 = (t294 + t323) ** 2
      t326 = 0.1D1 / t261 * z * t325
      t331 = 0.90D2 * wd * t257 * t166 * t326 + 0.180D3 * t187 * t326
      t335 = FJET(XB1, XB2, s, t2 * x1 * (-t47 - t154 + t221 + t222 + t2
     #23 - t225 - x2 + t41 + t232) * t166, -t159, -t2 * x1 * t239 * t166
     #, t163, s * t243 * x2 * x1 * t157 * t166, t331 * t126 * t77 / 0.16
     #0D3)
      rrgg2gght12s2e1 = t152 * t151 + t219 * t218 + t335 * t331 * t126 *
     # t77 / 0.160D3

      end function



      doubleprecision function rrgg2gght12s2e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t6 = x4 * 0.3141592653589793D1
      t7 = Sin(t6)
      t8 = t7 ** 2
      t9 = z ** 2
      t11 = 0.1D1 / t9 / z
      t12 = t8 * t11
      t13 = sqrt(x2)
      t14 = -0.1D1 + t13
      t16 = t13 + 0.1D1
      t22 = log(0.4D1 * t12 * t14 * t16 * x2 * x3 * t4)
      t23 = x3 * t13
      t25 = 0.2D1 * z * t23
      t26 = t13 * z
      t28 = cos(t6)
      t29 = x3 * t14
      t33 = Sqrt(t29 * t16 * z * t4)
      t37 = (t25 - t26 + 0.2D1 * t23 - t13 + 0.4D1 * t28 * t33) ** 2
      t39 = x2 * x3
      t40 = t12 * t4
      t43 = log(-0.4D1 * t40 * t39)
      t44 = x3 * z
      t46 = Sqrt(-t44 * t4)
      t47 = t46 ** 2
      t49 = t28 ** 2
      t55 = lh * wd
      t58 = -t37 + 0.16D2 * t47 * t49
      t62 = 0.1D1 / x2
      t67 = 0.1D1 / x1
      t68 = t67 * t62
      t71 = t49 * wd
      t72 = x1 ** 2
      t73 = x3 * t72
      t76 = log(-0.4D1 * t73 * t40)
      t91 = log(-0.4D1 * x3 * t11 * t8 * t4)
      t95 = t91 ** 2
      t98 = lh ** 2
      t100 = 0.3141592653589793D1 ** 2
      t108 = -(-0.90D2 * wd * (t22 * t37 - 0.16D2 * t43 * t47 * t49) + 0
     #.180D3 * t55 * t58) * t62 / 0.320D3 + 0.9D1 / 0.16D2 * wd * t58 * 
     #t68 + (-0.1440D4 * t71 * t76 * t47 - 0.2880D4 * lh * t49 * wd * t4
     #7) * t67 / 0.160D3 - (-0.180D3 * t91 * t47 * lh - 0.45D2 * t95 * t
     #47 + t47 * (-0.180D3 * t98 + 0.30D2 * t100)) * t49 * wd / 0.20D2
      t109 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t
     #108)
      t111 = x3 * x1
      t114 = -0.1D1 + x1
      t115 = t1 * t114
      t116 = x3 * s * t115
      t117 = t4 * s
      t120 = t117 * t115
      t121 = x1 * z
      t122 = -z - x1 + t121
      t123 = 0.1D1 / t122
      t124 = wd * t123
      t128 = Sqrt(x3 * t122 * t4)
      t129 = t128 ** 2
      t137 = t114 ** 2
      t142 = log(0.4D1 * t73 * t8 / t9 * t123 * t137 * t4)
      t156 = 0.9D1 * t124 * z * t49 * t129 * t68 + (-0.1440D4 * t71 * t1
     #42 * t123 * z * t129 - 0.2880D4 * t55 * t123 * z * t49 * t129) * t
     #67 / 0.160D3
      t157 = FJET(XB1, XB2, s, t2 * t111, -t116, -t117 * t1 * x1, t120, 
     #0.0D0, t156)
      t159 = t111 * z
      t160 = t39 * z
      t161 = t111 * x2
      t162 = x2 * z
      t163 = t111 * t162
      t168 = Sqrt(-t29 * t16 * t122 * t4)
      t170 = 0.2D1 * t28 * t13 * t168
      t175 = x1 * x2
      t176 = t175 * z
      t177 = z + x1 - t121 - t162 - t175 + t176 - t44 - t111 + t159 + t1
     #60 + t161 - t163 + t39 + t170
      t181 = t1 ** 2
      t188 = (z + x1 - t121 - t175 + t176) ** 2
      t189 = 0.1D1 / t188
      t192 = z * t28 * t168
      t197 = x1 * t13
      t200 = t72 * t13
      t203 = t72 * t9
      t211 = t13 * t9
      t214 = t13 * x2
      t215 = t214 * t72
      t218 = t214 * x1
      t226 = 0.4D1 * t175 * t192 + t25 + 0.2D1 * t111 * t13 - 0.2D1 * t1
     #97 * z + 0.4D1 * t200 * z - 0.2D1 * t203 * t13 + 0.3D1 * t197 * t9
     # + 0.4D1 * x1 * t28 * t168 + 0.2D1 * t211 * x3 - 0.2D1 * t215 * x3
     # - 0.2D1 * t218 * x3 - 0.2D1 * t215 * z - t218 * t9 + t215 * t9 + 
     #0.4D1 * t192
      t237 = t9 * x3
      t242 = t28 * t168
      t250 = 0.4D1 * t73 * t13 + 0.4D1 * t203 * t23 - 0.8D1 * t73 * t26 
     #- 0.6D1 * t111 * t211 + 0.4D1 * t215 * t44 + 0.2D1 * t218 * t237 -
     # 0.2D1 * t215 * t237 - 0.4D1 * t121 * t242 - 0.4D1 * t175 * t242 +
     # 0.4D1 * t111 * t26 + t218 - t211 + t215 - t197 - 0.2D1 * t200 - t
     #26
      t252 = (t226 + t250) ** 2
      t254 = z * t252 * t68
      t257 = FJET(XB1, XB2, s, t2 * x1 * (-t44 - t111 + t159 + t160 + t1
     #61 - t163 - x2 + t39 + t170) * t123, -t116, -t2 * x1 * t177 * t123
     #, t120, s * t181 * x2 * x1 * t114 * t123, -0.9D1 / 0.16D2 * t124 *
     # t189 * t254)
      rrgg2gght12s2e0 = t109 * t108 + t157 * t156 - 0.9D1 / 0.16D2 * t25
     #7 * wd * t123 * t189 * t254

      end function



      doubleprecision function rrgg2gght12s2em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t6 = x4 * 0.3141592653589793D1
      t7 = cos(t6)
      t8 = t7 ** 2
      t9 = t8 * wd
      t12 = Sqrt(-x3 * z * t4)
      t13 = t12 ** 2
      t14 = 0.1D1 / x1
      t20 = z ** 2
      t24 = Sin(t6)
      t25 = t24 ** 2
      t29 = log(-0.4D1 * x3 / t20 / z * t25 * t4)
      t36 = sqrt(x2)
      t37 = x3 * t36
      t48 = Sqrt(x3 * (-0.1D1 + t36) * (t36 + 0.1D1) * z * t4)
      t52 = (0.2D1 * t37 * z - t36 * z + 0.2D1 * t37 - t36 + 0.4D1 * t7 
     #* t48) ** 2
      t60 = 0.9D1 * t9 * t13 * t14 - (0.180D3 * t13 * lh + 0.90D2 * t29 
     #* t13) * t8 * wd / 0.20D2 + 0.9D1 / 0.32D2 * wd * (-t52 + 0.16D2 *
     # t13 * t8) / x2
      t61 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t6
     #0)
      t67 = t1 * (-0.1D1 + x1)
      t69 = t4 * s
      t74 = -z - x1 + x1 * z
      t75 = 0.1D1 / t74
      t79 = Sqrt(x3 * t74 * t4)
      t80 = t79 ** 2
      t85 = FJET(XB1, XB2, s, t2 * x1 * x3, -x3 * s * t67, -t69 * t1 * x
     #1, t69 * t67, 0.0D0, 0.9D1 * t9 * t75 * z * t80 * t14)
      rrgg2gght12s2em1 = t61 * t60 + 0.9D1 * t85 * t8 * wd * t75 * z * t
     #80 * t14

      end function



      doubleprecision function rrgg2gght12s2em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t2 = s * (-0.1D1 + z)
      t4 = -0.1D1 + x3
      t7 = cos(x4 * 0.3141592653589793D1)
      t8 = t7 ** 2
      t12 = Sqrt(-x3 * z * t4)
      t13 = t12 ** 2
      t16 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.
     #9D1 / 0.2D1 * t8 * wd * t13)
      rrgg2gght12s2em2 = 0.9D1 / 0.2D1 * t16 * t8 * wd * t13

      end function



      doubleprecision function rrgg2gght12s2em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgg2gght12s2em3 = 0.0D0

      end function



      doubleprecision function rrgg2gght12s2em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgg2gght12s2em4 = 0.0D0

      end function
