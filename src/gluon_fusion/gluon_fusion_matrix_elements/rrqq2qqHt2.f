  
      subroutine rrqq2qqht2
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrqq2qqht2s1e1  
      doubleprecision rrqq2qqht2s1e0  
      doubleprecision rrqq2qqht2s1em1  
      doubleprecision rrqq2qqht2s1em2  
      doubleprecision rrqq2qqht2s1em3  
      doubleprecision rrqq2qqht2s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrqq2qqht2s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrqq2qqht2s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrqq2qqht2s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrqq2qqht2s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrqq2qqht2s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrqq2qqht2s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrqq2qqht2s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = z - 0.1D1
      t4 = t3 * x1
      t5 = t2 * t4
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t8 = t2 * t7
      t9 = t1 ** 2
      t11 = t3 ** 2
      t14 = s * t9 * t11 * x1 * t6
      t15 = t11 * t3
      t16 = t15 * x1
      t17 = 0.1D1 / z
      t18 = t6 * t17
      t19 = t16 * t18
      t20 = x2 * 0.3141592653589793D1
      t21 = cos(t20)
      t22 = t21 ** 2
      t23 = wd * t22
      t24 = sin(t20)
      t25 = t24 ** 2
      t26 = z ** 2
      t27 = 0.1D1 / t26
      t28 = t25 * t27
      t29 = t11 ** 2
      t30 = t28 * t29
      t31 = x1 ** 2
      t32 = t6 ** 2
      t33 = t31 * t32
      t34 = t9 ** 2
      t36 = t33 * x4 * t34
      t39 = log(0.4D1 * t30 * t36)
      t40 = t39 ** 2
      t42 = 0.1D1 / (-0.2D1 + t1)
      t48 = t6 * lh
      t50 = t16 * t48 * t17
      t56 = 0.3141592653589793D1 ** 2
      t58 = lh ** 2
      t60 = -0.30D2 * t56 + 0.180D3 * t58
      t61 = t6 * t60
      t63 = t17 * wd
      t65 = t22 * t42 * t34
      t66 = t63 * t65
      t67 = t16 * t61 * t66
      t69 = 0.1D1 / x4
      t82 = log(0.4D1 * t30 * t33 * t34)
      t84 = x1 * t6
      t87 = t82 ** 2
      t101 = x3 * t25
      t106 = log(0.4D1 * t101 * t27 * t29 * t36)
      t111 = t16 * t48
      t115 = 0.1D1 / x3
      t120 = t29 * t31
      t125 = log(0.4D1 * t101 * t27 * t120 * t32 * t34)
      t126 = t125 ** 2
      t140 = 0.8D1 / 0.405D3 * (-0.45D2 * t19 * t23 * t40 * t42 * t34 - 
     #0.180D3 * t50 * t23 * t39 * t42 * t34 - t67) * t69 - 0.8D1 / 0.405
     #D3 * (t16 * t6 * (-0.2884936567583026D3 - 0.120D3 * t58 * lh + 0.6
     #0D2 * lh * t56) - t82 * t15 * t84 * t60 - 0.90D2 * t87 * t15 * t84
     # * lh - 0.15D2 * t87 * t82 * t15 * t84) * wd * t17 * t65 - 0.2D1 /
     # 0.405D3 * (-0.360D3 * t19 * wd * t106 * t65 - 0.720D3 * t111 * t6
     #6) * t115 * t69 + 0.8D1 / 0.405D3 * (-0.45D2 * t19 * t23 * t126 * 
     #t42 * t34 - 0.180D3 * t50 * t23 * t125 * t42 * t34 - t67) * t115
      t141 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t140)
      t143 = sqrt(x4)
      t144 = -0.1D1 + t143
      t145 = t143 + 0.1D1
      t146 = t144 * t145
      t147 = KAPPA2(x1, x2, 0.0D0, -t146, z)
      t148 = s * t147
      t149 = t148 * t4
      t152 = t6 * t144 * t145
      t153 = t148 * t3 * t152
      t154 = t7 * x4
      t155 = t148 * t154
      t156 = t147 ** 2
      t161 = s * t156 * t11 * t84 * (-0.1D1 + x4)
      t163 = t16 * t18 * wd
      t164 = x4 * t29
      t166 = t156 ** 2
      t171 = log(-0.4D1 * t164 * t33 * t28 * t146 * t166)
      t172 = t171 ** 2
      t174 = Sqrt(-t146)
      t175 = t174 ** 2
      t177 = 0.1D1 / (-0.2D1 + t147)
      t179 = t175 * t177 * t166
      t188 = t16 * t61 * t17
      t189 = t23 * t179
      t202 = log(-0.4D1 * t164 * t33 * t25 * t27 * t144 * t145 * t166 * 
     #x3)
      t215 = 0.8D1 / 0.405D3 * (0.45D2 * t163 * t22 * t172 * t179 + 0.18
     #0D3 * t50 * t23 * t171 * t179 + t188 * t189) * t69 - 0.2D1 / 0.405
     #D3 * (0.360D3 * t163 * t202 * t175 * t22 * t177 * t166 + 0.720D3 *
     # t50 * t189) * t115 * t69
      t216 = FJET(XB1, XB2, s, 0.0D0, t149, t153, -t155, t161, t215)
      t218 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t140)
      t220 = FJET(XB1, XB2, s, t149, 0.0D0, -t155, t153, t161, t215)
      t222 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t223 = s * t222
      t224 = t4 * x3
      t225 = t223 * t224
      t227 = sqrt(x3)
      t228 = -0.1D1 + t227
      t230 = t227 + 0.1D1
      t231 = x1 * t228 * t230
      t232 = t223 * t3 * t231
      t233 = t223 * t7
      t234 = t222 ** 2
      t239 = s * t234 * t11 * t84 * (-0.1D1 + x3)
      t240 = t234 ** 2
      t244 = t228 * t230
      t245 = t244 * x3
      t246 = t28 * t245
      t249 = log(-0.4D1 * x4 * t240 * t120 * t32 * t246)
      t251 = 0.1D1 / (-0.2D1 + t222)
      t254 = Sqrt(-t244)
      t255 = t254 ** 2
      t256 = t240 * t22 * t255
      t261 = wd * t251 * t256
      t272 = log(-0.4D1 * t240 * t29 * t33 * t246)
      t273 = t272 ** 2
      t276 = t251 * t240 * t255
      t288 = -0.2D1 / 0.405D3 * (0.360D3 * t163 * t249 * t251 * t256 + 0
     #.720D3 * t50 * t261) * t115 * t69 + 0.8D1 / 0.405D3 * (0.45D2 * t1
     #63 * t22 * t273 * t276 + 0.180D3 * t50 * t23 * t272 * t276 + t188 
     #* t261) * t115
      t289 = FJET(XB1, XB2, s, t225, -t232, -t233, 0.0D0, t239, t288)
      t291 = KAPPA2(x1, x2, x3, -t146, z)
      t292 = s * t291
      t293 = t292 * t224
      t294 = t292 * t3
      t295 = t294 * t231
      t296 = t294 * t152
      t297 = t292 * t154
      t298 = t291 ** 2
      t305 = Sqrt(t244 * t146)
      t311 = s * t298 * t11 * t84 * (x3 - 0.1D1 + x4 - 0.2D1 * x3 * x4 +
     # 0.2D1 * t21 * t227 * t143 * t305)
      t316 = t298 ** 2
      t321 = log(0.4D1 * t164 * t31 * t32 * t25 * t27 * t146 * t316 * t2
     #45)
      t327 = (t227 * t143 - 0.2D1 * t21 * t305) ** 2
      t331 = t327 * t316 / (-0.2D1 + t291)
      t338 = -0.90D2 * t19 * wd * t321 * t331 - 0.180D3 * t111 * t63 * t
     #331
      t341 = 0.2D1 / 0.405D3 * t338 * t115 * t69
      t342 = FJET(XB1, XB2, s, t293, -t295, t296, -t297, t311, -t341)
      t344 = t115 * t69
      t347 = FJET(XB1, XB2, s, -t232, t225, 0.0D0, -t233, t239, t288)
      t349 = FJET(XB1, XB2, s, -t295, t293, -t297, t296, t311, -t341)
      rrqq2qqht2s1e1 = t141 * t140 + t216 * t215 + t218 * t140 + t220 * 
     #t215 + t289 * t288 - 0.2D1 / 0.405D3 * t342 * t338 * t344 + t347 *
     # t288 - 0.2D1 / 0.405D3 * t349 * t338 * t344

      end function



      doubleprecision function rrqq2qqht2s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = z - 0.1D1
      t4 = t3 * x1
      t5 = t2 * t4
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t8 = t2 * t7
      t9 = t1 ** 2
      t11 = t3 ** 2
      t14 = s * t9 * t11 * x1 * t6
      t15 = t11 * t3
      t16 = t15 * x1
      t17 = 0.1D1 / z
      t18 = t6 * t17
      t19 = t16 * t18
      t20 = x2 * 0.3141592653589793D1
      t21 = cos(t20)
      t22 = t21 ** 2
      t23 = wd * t22
      t24 = sin(t20)
      t25 = t24 ** 2
      t26 = z ** 2
      t27 = 0.1D1 / t26
      t28 = t25 * t27
      t29 = t11 ** 2
      t30 = t28 * t29
      t31 = x1 ** 2
      t32 = t6 ** 2
      t33 = t31 * t32
      t34 = t9 ** 2
      t39 = log(0.4D1 * t30 * t33 * x4 * t34)
      t41 = 0.1D1 / (-0.2D1 + t1)
      t47 = t6 * lh
      t50 = t22 * t41
      t51 = t50 * t34
      t54 = 0.180D3 * t16 * t47 * t17 * wd * t51
      t56 = 0.1D1 / x4
      t60 = t16 * t18 * wd
      t61 = 0.1D1 / x3
      t74 = log(0.4D1 * x3 * t25 * t27 * t29 * t31 * t32 * t34)
      t83 = 0.3141592653589793D1 ** 2
      t85 = lh ** 2
      t93 = log(0.4D1 * t30 * t33 * t34)
      t95 = x1 * t6
      t99 = t93 ** 2
      t108 = 0.8D1 / 0.405D3 * (0.90D2 * t19 * t23 * t39 * t41 * t34 + t
     #54) * t56 - 0.16D2 / 0.9D1 * t60 * t50 * t34 * t61 * t56 + 0.8D1 /
     # 0.405D3 * (0.90D2 * t19 * t23 * t74 * t41 * t34 + t54) * t61 - 0.
     #8D1 / 0.405D3 * (t16 * t6 * (-0.30D2 * t83 + 0.180D3 * t85) + 0.18
     #0D3 * t93 * t15 * t95 * lh + 0.45D2 * t99 * t15 * t95) * wd * t17 
     #* t51
      t109 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t108)
      t111 = sqrt(x4)
      t112 = -0.1D1 + t111
      t113 = t111 + 0.1D1
      t114 = t112 * t113
      t115 = KAPPA2(x1, x2, 0.0D0, -t114, z)
      t116 = s * t115
      t117 = t116 * t4
      t120 = t6 * t112 * t113
      t121 = t116 * t3 * t120
      t122 = t7 * x4
      t123 = t116 * t122
      t124 = t115 ** 2
      t129 = s * t124 * t11 * t95 * (-0.1D1 + x4)
      t132 = t124 ** 2
      t137 = log(-0.4D1 * x4 * t29 * t33 * t28 * t114 * t132)
      t139 = Sqrt(-t114)
      t140 = t139 ** 2
      t142 = 0.1D1 / (-0.2D1 + t115)
      t149 = t16 * t47 * t17
      t166 = 0.8D1 / 0.405D3 * (-0.90D2 * t60 * t22 * t137 * t140 * t142
     # * t132 - 0.180D3 * t149 * wd * t140 * t22 * t142 * t132) * t56 + 
     #0.16D2 / 0.9D1 * t60 * t140 * t22 * t142 * t132 * t61 * t56
      t167 = FJET(XB1, XB2, s, 0.0D0, t117, t121, -t123, t129, t166)
      t169 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t108)
      t171 = FJET(XB1, XB2, s, t117, 0.0D0, -t123, t121, t129, t166)
      t173 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t174 = s * t173
      t175 = t4 * x3
      t176 = t174 * t175
      t178 = sqrt(x3)
      t179 = -0.1D1 + t178
      t181 = t178 + 0.1D1
      t182 = x1 * t179 * t181
      t183 = t174 * t3 * t182
      t184 = t174 * t7
      t185 = t173 ** 2
      t190 = s * t185 * t11 * t95 * (-0.1D1 + x3)
      t192 = 0.1D1 / (-0.2D1 + t173)
      t193 = t185 ** 2
      t194 = t192 * t193
      t196 = t179 * t181
      t197 = Sqrt(-t196)
      t198 = t197 ** 2
      t210 = log(-0.4D1 * t193 * t29 * t33 * t28 * t196 * x3)
      t225 = 0.16D2 / 0.9D1 * t60 * t194 * t22 * t198 * t61 * t56 + 0.8D
     #1 / 0.405D3 * (-0.90D2 * t60 * t22 * t210 * t194 * t198 - 0.180D3 
     #* t149 * wd * t192 * t193 * t22 * t198) * t61
      t226 = FJET(XB1, XB2, s, t176, -t183, -t184, 0.0D0, t190, t225)
      t228 = KAPPA2(x1, x2, x3, -t114, z)
      t229 = s * t228
      t230 = t229 * t175
      t231 = t229 * t3
      t232 = t231 * t182
      t233 = t231 * t120
      t234 = t229 * t122
      t235 = t228 ** 2
      t242 = Sqrt(t196 * t114)
      t248 = s * t235 * t11 * t95 * (x3 - 0.1D1 + x4 - 0.2D1 * x3 * x4 +
     # 0.2D1 * t21 * t178 * t111 * t242)
      t253 = (t178 * t111 - 0.2D1 * t21 * t242) ** 2
      t254 = t235 ** 2
      t259 = 0.1D1 / (-0.2D1 + t228) * t61 * t56
      t262 = 0.4D1 / 0.9D1 * t60 * t253 * t254 * t259
      t263 = FJET(XB1, XB2, s, t230, -t232, t233, -t234, t248, -t262)
      t265 = t95 * t17
      t269 = wd * t253 * t254 * t259
      t272 = FJET(XB1, XB2, s, -t183, t176, 0.0D0, -t184, t190, t225)
      t274 = FJET(XB1, XB2, s, -t232, t230, -t234, t233, t248, -t262)
      rrqq2qqht2s1e0 = t109 * t108 + t167 * t166 + t169 * t108 + t171 * 
     #t166 + t226 * t225 - 0.4D1 / 0.9D1 * t263 * t15 * t265 * t269 + t2
     #72 * t225 - 0.4D1 / 0.9D1 * t274 * t15 * t265 * t269

      end function



      doubleprecision function rrqq2qqht2s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = z - 0.1D1
      t4 = t3 * x1
      t5 = t2 * t4
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t8 = t2 * t7
      t9 = t1 ** 2
      t11 = t3 ** 2
      t14 = s * t9 * t11 * x1 * t6
      t15 = t11 * t3
      t16 = t15 * x1
      t17 = 0.1D1 / z
      t18 = t6 * t17
      t19 = t16 * t18
      t20 = x2 * 0.3141592653589793D1
      t21 = cos(t20)
      t22 = t21 ** 2
      t23 = wd * t22
      t25 = 0.1D1 / (-0.2D1 + t1)
      t26 = t9 ** 2
      t27 = t25 * t26
      t28 = 0.1D1 / x3
      t36 = sin(t20)
      t37 = t36 ** 2
      t38 = z ** 2
      t41 = t11 ** 2
      t43 = x1 ** 2
      t44 = t6 ** 2
      t49 = log(0.4D1 * t37 / t38 * t41 * t43 * t44 * t26)
      t51 = x1 * t6
      t61 = 0.1D1 / x4
      t66 = -0.16D2 / 0.9D1 * t19 * t23 * t27 * t28 - 0.8D1 / 0.405D3 * 
     #(-0.180D3 * t16 * t6 * lh - 0.90D2 * t49 * t15 * t51) * wd * t17 *
     # t22 * t25 * t26 - 0.16D2 / 0.9D1 * t19 * t23 * t27 * t61
      t67 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t66)
      t69 = sqrt(x4)
      t70 = -0.1D1 + t69
      t71 = t69 + 0.1D1
      t72 = t70 * t71
      t73 = KAPPA2(x1, x2, 0.0D0, -t72, z)
      t74 = s * t73
      t75 = t74 * t4
      t79 = t74 * t3 * t6 * t70 * t71
      t81 = t74 * t7 * x4
      t82 = t73 ** 2
      t87 = s * t82 * t11 * t51 * (-0.1D1 + x4)
      t89 = t16 * t18 * wd
      t90 = Sqrt(-t72)
      t91 = t90 ** 2
      t95 = t82 ** 2
      t97 = 0.1D1 / (-0.2D1 + t73) * t95 * t61
      t100 = 0.16D2 / 0.9D1 * t89 * t91 * t22 * t97
      t101 = FJET(XB1, XB2, s, 0.0D0, t75, t79, -t81, t87, t100)
      t103 = t51 * t17
      t107 = wd * t91 * t22 * t97
      t110 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t66)
      t112 = FJET(XB1, XB2, s, t75, 0.0D0, -t81, t79, t87, t100)
      t117 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t118 = s * t117
      t120 = t118 * t4 * x3
      t122 = sqrt(x3)
      t123 = -0.1D1 + t122
      t125 = t122 + 0.1D1
      t127 = t118 * t3 * x1 * t123 * t125
      t128 = t118 * t7
      t129 = t117 ** 2
      t134 = s * t129 * t11 * t51 * (-0.1D1 + x3)
      t136 = 0.1D1 / (-0.2D1 + t117)
      t137 = t129 ** 2
      t140 = Sqrt(-t123 * t125)
      t141 = t140 ** 2
      t143 = t22 * t141 * t28
      t146 = 0.16D2 / 0.9D1 * t89 * t136 * t137 * t143
      t147 = FJET(XB1, XB2, s, t120, -t127, -t128, 0.0D0, t134, t146)
      t152 = wd * t136 * t137 * t143
      t155 = FJET(XB1, XB2, s, -t127, t120, 0.0D0, -t128, t134, t146)
      rrqq2qqht2s1em1 = t67 * t66 + 0.16D2 / 0.9D1 * t101 * t15 * t103 *
     # t107 + t110 * t66 + 0.16D2 / 0.9D1 * t112 * t15 * t103 * t107 + 0
     #.16D2 / 0.9D1 * t147 * t15 * t103 * t152 + 0.16D2 / 0.9D1 * t155 *
     # t15 * t103 * t152

      end function



      doubleprecision function rrqq2qqht2s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = z - 0.1D1
      t5 = t2 * t3 * x1
      t6 = -0.1D1 + x1
      t8 = t2 * t3 * t6
      t9 = t1 ** 2
      t11 = t3 ** 2
      t14 = s * t9 * t11 * x1 * t6
      t15 = t11 * t3
      t17 = 0.1D1 / z
      t21 = cos(x2 * 0.3141592653589793D1)
      t22 = t21 ** 2
      t25 = 0.1D1 / (-0.2D1 + t1)
      t26 = t9 ** 2
      t30 = 0.16D2 / 0.9D1 * t15 * x1 * t6 * t17 * wd * t22 * t25 * t26
      t31 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, -t30)
      t33 = x1 * t6
      t38 = t17 * wd * t22 * t25 * t26
      t40 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, -t30)
      rrqq2qqht2s1em2 = -0.16D2 / 0.9D1 * t31 * t15 * t33 * t38 - 0.16D2
     # / 0.9D1 * t40 * t15 * t33 * t38

      end function



      doubleprecision function rrqq2qqht2s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrqq2qqht2s1em3 = 0.0D0

      end function



      doubleprecision function rrqq2qqht2s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrqq2qqht2s1em4 = 0.0D0

      end function
