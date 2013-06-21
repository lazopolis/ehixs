  
      subroutine gbgbH8n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision gbgbH8n1e1  
      doubleprecision gbgbH8n1e0  
      doubleprecision gbgbH8n1em1  
      doubleprecision gbgbH8n1em2  
      doubleprecision gbgbH8n1em3  
      doubleprecision gbgbH8n1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=gbgbH8n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=gbgbH8n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=gbgbH8n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=gbgbH8n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=gbgbH8n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=gbgbH8n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function gbgbH8n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = x2 * x3
      t4 = 0.2D1 * t3
      t5 = x4 * 0.3141592653589793D1
      t6 = cos(t5)
      t7 = -0.1D1 + x2
      t9 = -0.1D1 + x3
      t12 = Sqrt(x2 * t7 * x3 * t9)
      t14 = 0.2D1 * t6 * t12
      t16 = t2 * (0.1D1 - x2 - x3 + t4 + t14)
      t18 = t2 * (-x3 + t4 - x2 + t14)
      t19 = t1 ** 2
      t20 = z * t19
      t21 = t20 * wd
      t23 = z * lh
      t24 = t19 * wd
      t26 = 0.180D3 * t23 * t24
      t27 = -0.90D2 * t21 - t26
      t28 = x2 * z
      t30 = (0.1D1 + t28 - x2) ** 2
      t31 = 0.1D1 / t30
      t32 = t27 * t31
      t33 = Sin(t5)
      t34 = t33 ** 2
      t36 = z ** 2
      t37 = 0.1D1 / t36
      t38 = t37 * t7
      t42 = log(0.4D1 * t3 * t34 * t38 * t9)
      t43 = t42 * t9
      t47 = wd * t31
      t49 = t42 ** 2
      t55 = lh ** 2
      t57 = 0.3141592653589793D1 ** 2
      t59 = 0.180D3 * t55 - 0.30D2 * t57
      t60 = z * t59
      t62 = t26 + t60 * t24
      t63 = t62 * t31
      t64 = -t9
      t68 = 0.1D1 / x3
      t71 = x1 ** 2
      t72 = t3 * t71
      t73 = t34 * t37
      t78 = log(0.4D1 * t72 * t73 * t7 * t9)
      t80 = t78 * t31 * t9
      t87 = t32 * t9
      t90 = 0.1D1 / x1
      t94 = -(t32 * (0.2D1 * t43 - 0.2D1 + 0.2D1 * x3) + 0.90D2 * t20 * 
     #t47 * (-0.2D1 * t43 - t49 * t9) + 0.2D1 * t63 * t64) * t68 / 0.270
     #D3 + (0.90D2 * t20 * wd * (-0.2D1 * t80 - 0.2D1 * t31 * t9) + 0.2D
     #1 * t87) * t90 * t68 / 0.135D3
      t95 = FJET(XB1, XB2, s, 0.0D0, t16, 0.0D0, -t18, 0.0D0, t94)
      t98 = x2 * s * t1
      t99 = t7 * s
      t100 = t99 * t1
      t104 = log(-0.4D1 * x2 * t34 * t38)
      t105 = t104 * z
      t108 = t104 ** 2
      t109 = t108 * z
      t113 = (0.180D3 * t105 * lh + t60 + 0.45D2 * t109) * t19 * wd
      t130 = (-t113 + (-t105 * t59 - 0.90D2 * t109 * lh + z * (-0.288493
     #6567583026D3 - 0.120D3 * t55 * lh + 0.60D2 * lh * t57) - 0.15D2 * 
     #t108 * t104 * z) * t19 * wd) * t31
      t132 = t73 * t7
      t135 = log(-0.4D1 * t3 * t132)
      t138 = t135 ** 2
      t142 = 0.9D1 * t63
      t148 = log(-0.4D1 * t72 * t132)
      t158 = x2 * t71
      t161 = log(-0.4D1 * t158 * t132)
      t165 = t161 ** 2
      t173 = t130 / 0.30D2 - (0.9D1 * t32 * t135 - 0.405D3 * t20 * t47 *
     # t138 - t142) * t68 / 0.270D3 + (-0.810D3 * t20 * wd * t148 * t31 
     #+ 0.9D1 * t32) * t90 * t68 / 0.135D3 - (0.9D1 * t27 * t161 * t31 -
     # 0.405D3 * t20 * wd * t165 * t31 - t142) * t90 / 0.135D3
      t174 = FJET(XB1, XB2, s, 0.0D0, t98, 0.0D0, -t100, 0.0D0, t173)
      t194 = -(0.9D1 * t32 * t43 - 0.405D3 * t21 * t31 * t49 * t9 + 0.9D
     #1 * t63 * t64) * t68 / 0.270D3 + (-0.810D3 * t21 * t80 + 0.9D1 * t
     #87) * t90 * t68 / 0.135D3
      t195 = FJET(XB1, XB2, s, 0.0D0, -t18, 0.0D0, t16, 0.0D0, t194)
      t214 = 0.2D1 * t63
      t229 = t161 * t31
      t242 = -(-(-0.180D3 * t23 - 0.90D2 * t105) * t19 * wd + t113) * t3
     #1 / 0.135D3 + t130 / 0.135D3 - (t32 * (0.2D1 * t135 + 0.2D1) + 0.9
     #0D2 * t20 * t47 * (-0.2D1 * t135 - t138) - t214) * t68 / 0.270D3 +
     # (0.90D2 * t20 * wd * (-0.2D1 * t31 - 0.2D1 * t148 * t31) + 0.2D1 
     #* t32) * t90 * t68 / 0.135D3 - (t27 * (0.2D1 * t31 + 0.2D1 * t229)
     # + 0.90D2 * t20 * wd * (-0.2D1 * t229 - t165 * t31) - t214) * t90 
     #/ 0.135D3
      t243 = FJET(XB1, XB2, s, 0.0D0, -t100, 0.0D0, t98, 0.0D0, t242)
      t245 = -0.1D1 + x1
      t247 = x1 * z
      t248 = 0.1D1 - x1 + t247
      t249 = 0.1D1 / t248
      t251 = t2 * t245 * x2 * t249
      t252 = t2 * x1
      t254 = t99 * t1 * t245
      t259 = s * t19 * x2 * t245 * x1 * t249
      t261 = t3 * t71 * t34
      t262 = t37 * t249
      t263 = t245 ** 2
      t264 = t263 * t7
      t265 = t262 * t264
      t268 = log(-0.4D1 * t261 * t265)
      t270 = x2 * x1
      t271 = t270 * z
      t273 = (-0.1D1 + x2 - t270 - t28 + t271 - t247 + x1) ** 2
      t274 = 0.1D1 / t273
      t275 = t248 * t274
      t276 = t268 * t245 * t275
      t279 = t27 * t245
      t280 = t279 * t275
      t288 = log(-0.4D1 * t158 * t34 * t265)
      t290 = t245 * t248
      t291 = t290 * t274
      t294 = t288 ** 2
      t296 = t294 * t245 * t275
      t300 = t62 * t245 * t275
      t305 = (-0.810D3 * t21 * t276 + 0.9D1 * t280) * t90 * t68 / 0.135D
     #3 - (0.9D1 * t27 * t288 * t291 - 0.405D3 * t21 * t296 - 0.9D1 * t3
     #00) * t90 / 0.135D3
      t306 = FJET(XB1, XB2, s, 0.0D0, -t251, t252, t254, -t259, t305)
      t318 = t288 * t245 * t275
      t331 = (0.90D2 * t20 * wd * (-0.2D1 * t276 - 0.2D1 * t291) + 0.2D1
     # * t280) * t90 * t68 / 0.135D3 - (t27 * (0.2D1 * t291 + 0.2D1 * t3
     #18) + 0.90D2 * t20 * wd * (-0.2D1 * t318 - t296) - 0.2D1 * t300) *
     # t90 / 0.135D3
      t332 = FJET(XB1, XB2, s, t252, t254, 0.0D0, -t251, -t259, t331)
      t334 = x1 * x3
      t335 = t2 * t334
      t336 = t334 * z
      t337 = t3 * x1
      t338 = t3 * t247
      t343 = Sqrt(x3 * t7 * t248 * x2 * t9)
      t345 = 0.2D1 * t6 * t343
      t349 = t2 * t245 * (-x3 + t334 - t336 + t4 - t337 + t338 - x2 + t3
     #45) * t249
      t352 = t9 * s * t1 * x1
      t353 = 0.1D1 - x1 + t247 - x2 + t270 - t271 - x3 + t334 - t336 + t
     #4 - t337 + t338 + t345
      t356 = t2 * t245 * t353 * t249
      t361 = log(0.4D1 * t261 * t262 * t264 * t9)
      t365 = t290 * t9 * t274
      t369 = t248 * t9 * t274
      t370 = t279 * t369
      t372 = -0.810D3 * t20 * wd * t361 * t365 + 0.9D1 * t370
      t376 = FJET(XB1, XB2, s, t335, t349, -t352, -t356, -t259, t372 * t
     #90 * t68 / 0.135D3)
      t378 = t90 * t68
      t389 = 0.90D2 * t20 * wd * (-0.2D1 * t365 - 0.2D1 * t361 * t245 * 
     #t369) + 0.2D1 * t370
      t393 = FJET(XB1, XB2, s, -t352, -t356, t335, t349, -t259, t389 * t
     #90 * t68 / 0.135D3)
      gbgbH8n1e1 = t95 * t94 + t174 * t173 + t195 * t194 + t243 * t242 +
     # t306 * t305 + t332 * t331 + t376 * t372 * t378 / 0.135D3 + t393 *
     # t389 * t378 / 0.135D3

      end function



      doubleprecision function gbgbH8n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = x2 * x3
      t4 = 0.2D1 * t3
      t5 = x4 * 0.3141592653589793D1
      t6 = cos(t5)
      t7 = -0.1D1 + x2
      t9 = -0.1D1 + x3
      t12 = Sqrt(x2 * t7 * x3 * t9)
      t14 = 0.2D1 * t6 * t12
      t16 = t2 * (0.1D1 - x2 - x3 + t4 + t14)
      t18 = t2 * (-x3 + t4 - x2 + t14)
      t19 = t1 ** 2
      t20 = z * t19
      t21 = t20 * wd
      t22 = x2 * z
      t24 = (0.1D1 + t22 - x2) ** 2
      t25 = 0.1D1 / t24
      t27 = 0.1D1 / x1
      t28 = 0.1D1 / x3
      t29 = t27 * t28
      t31 = t21 * t25 * t9 * t29
      t33 = wd * t25
      t34 = Sin(t5)
      t35 = t34 ** 2
      t37 = z ** 2
      t38 = 0.1D1 / t37
      t39 = t38 * t7
      t43 = log(0.4D1 * t3 * t35 * t39 * t9)
      t50 = 0.90D2 * t21
      t51 = z * lh
      t52 = t19 * wd
      t55 = -t50 - 0.180D3 * t51 * t52
      t56 = t55 * t25
      t57 = -t9
      t63 = 0.4D1 / 0.3D1 * t31 - (0.90D2 * t20 * t33 * (0.2D1 * t43 * t
     #9 - 0.2D1 + 0.2D1 * x3) + 0.2D1 * t56 * t57) * t28 / 0.270D3
      t64 = FJET(XB1, XB2, s, 0.0D0, t16, 0.0D0, -t18, 0.0D0, t63)
      t67 = x2 * s * t1
      t68 = t7 * s
      t69 = t68 * t1
      t71 = t35 * t38 * t7
      t74 = log(-0.4D1 * t3 * t71)
      t78 = 0.9D1 * t56
      t86 = log(-0.4D1 * x2 * t35 * t39)
      t87 = t86 * z
      t91 = (-0.180D3 * t51 - 0.90D2 * t87) * t19 * wd
      t94 = lh ** 2
      t96 = 0.3141592653589793D1 ** 2
      t100 = t86 ** 2
      t107 = (-t91 + (0.180D3 * t87 * lh + z * (0.180D3 * t94 - 0.30D2 *
     # t96) + 0.45D2 * t100 * z) * t19 * wd) * t25
      t111 = t21 * t25 * t27 * t28
      t113 = x1 ** 2
      t114 = x2 * t113
      t117 = log(-0.4D1 * t114 * t71)
      t125 = -(0.810D3 * t20 * t33 * t74 - t78) * t28 / 0.270D3 + t107 /
     # 0.30D2 + 0.6D1 * t111 - (0.810D3 * t20 * wd * t117 * t25 - t78) *
     # t27 / 0.135D3
      t126 = FJET(XB1, XB2, s, 0.0D0, t67, 0.0D0, -t69, 0.0D0, t125)
      t138 = 0.6D1 * t31 - (0.810D3 * t21 * t25 * t43 * t9 + 0.9D1 * t56
     # * t57) * t28 / 0.270D3
      t139 = FJET(XB1, XB2, s, 0.0D0, -t18, 0.0D0, t16, 0.0D0, t138)
      t146 = 0.2D1 * t56
      t164 = -(0.90D2 * t20 * t33 * (0.2D1 * t74 + 0.2D1) - t146) * t28 
     #/ 0.270D3 - (-t50 + t91) * t25 / 0.135D3 + t107 / 0.135D3 + 0.4D1 
     #/ 0.3D1 * t111 - (0.90D2 * t20 * wd * (0.2D1 * t25 + 0.2D1 * t117 
     #* t25) - t146) * t27 / 0.135D3
      t165 = FJET(XB1, XB2, s, 0.0D0, -t69, 0.0D0, t67, 0.0D0, t164)
      t167 = -0.1D1 + x1
      t169 = x1 * z
      t170 = 0.1D1 - x1 + t169
      t171 = 0.1D1 / t170
      t173 = t2 * t167 * x2 * t171
      t174 = t2 * x1
      t176 = t68 * t1 * t167
      t181 = s * t19 * x2 * t167 * x1 * t171
      t183 = t20 * wd * t167
      t184 = x2 * x1
      t185 = t184 * z
      t187 = (-0.1D1 + x2 - t184 - t22 + t185 - t169 + x1) ** 2
      t188 = 0.1D1 / t187
      t189 = t170 * t188
      t191 = t183 * t189 * t29
      t195 = t167 ** 2
      t200 = log(-0.4D1 * t114 * t35 * t38 * t171 * t195 * t7)
      t202 = t200 * t167 * t189
      t206 = t55 * t167 * t189
      t211 = 0.6D1 * t191 - (0.810D3 * t21 * t202 - 0.9D1 * t206) * t27 
     #/ 0.135D3
      t212 = FJET(XB1, XB2, s, 0.0D0, -t173, t174, t176, -t181, t211)
      t226 = 0.4D1 / 0.3D1 * t191 - (0.90D2 * t20 * wd * (0.2D1 * t167 *
     # t170 * t188 + 0.2D1 * t202) - 0.2D1 * t206) * t27 / 0.135D3
      t227 = FJET(XB1, XB2, s, t174, t176, 0.0D0, -t173, -t181, t226)
      t229 = x1 * x3
      t230 = t2 * t229
      t231 = t229 * z
      t232 = t3 * x1
      t233 = t3 * t169
      t238 = Sqrt(x3 * t7 * t170 * x2 * t9)
      t240 = 0.2D1 * t6 * t238
      t244 = t2 * t167 * (-x3 + t229 - t231 + t4 - t232 + t233 - x2 + t2
     #40) * t171
      t247 = t9 * s * t1 * x1
      t248 = 0.1D1 - x1 + t169 - x2 + t184 - t185 - x3 + t229 - t231 + t
     #4 - t232 + t233 + t240
      t251 = t2 * t167 * t248 * t171
      t255 = t170 * t9 * t188 * t27 * t28
      t256 = t183 * t255
      t258 = FJET(XB1, XB2, s, t230, t244, -t247, -t251, -t181, 0.6D1 * 
     #t256)
      t260 = t52 * t167
      t265 = FJET(XB1, XB2, s, -t247, -t251, t230, t244, -t181, 0.4D1 / 
     #0.3D1 * t256)
      gbgbH8n1e0 = t64 * t63 + t126 * t125 + t139 * t138 + t165 * t164 +
     # t212 * t211 + t227 * t226 + 0.6D1 * t258 * z * t260 * t255 + 0.4D
     #1 / 0.3D1 * t265 * z * t260 * t255

      end function



      doubleprecision function gbgbH8n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = 0.2D1 * x2 * x3
      t5 = x4 * 0.3141592653589793D1
      t6 = cos(t5)
      t7 = -0.1D1 + x2
      t9 = -0.1D1 + x3
      t12 = Sqrt(x2 * t7 * x3 * t9)
      t14 = 0.2D1 * t6 * t12
      t16 = t2 * (0.1D1 - x2 - x3 + t4 + t14)
      t18 = t2 * (-x3 + t4 - x2 + t14)
      t19 = t1 ** 2
      t20 = z * t19
      t21 = t20 * wd
      t22 = x2 * z
      t24 = (0.1D1 + t22 - x2) ** 2
      t25 = 0.1D1 / t24
      t26 = -t9
      t27 = 0.2D1 * t26
      t29 = 0.1D1 / x3
      t33 = FJET(XB1, XB2, s, 0.0D0, t16, 0.0D0, -t18, 0.0D0, -t21 * t25
     # * t27 * t29 / 0.3D1)
      t36 = wd * t25
      t42 = x2 * s * t1
      t43 = t7 * s
      t44 = t43 * t1
      t48 = Sin(t5)
      t49 = t48 ** 2
      t51 = z ** 2
      t56 = log(-0.4D1 * x2 * t49 / t51 * t7)
      t63 = (-0.90D2 * t21 + (-0.180D3 * z * lh - 0.90D2 * t56 * z) * t1
     #9 * wd) * t25
      t66 = t20 * t36 * t29
      t68 = 0.1D1 / x1
      t70 = t20 * t36 * t68
      t72 = t63 / 0.30D2 + 0.3D1 * t66 + 0.6D1 * t70
      t73 = FJET(XB1, XB2, s, 0.0D0, t42, 0.0D0, -t44, 0.0D0, t72)
      t75 = 0.9D1 * t26
      t80 = FJET(XB1, XB2, s, 0.0D0, -t18, 0.0D0, t16, 0.0D0, -t21 * t25
     # * t75 * t29 / 0.3D1)
      t92 = -0.2D1 / 0.3D1 * t20 * t36 + t63 / 0.135D3 + 0.2D1 / 0.3D1 *
     # t66 + 0.4D1 / 0.3D1 * t70
      t93 = FJET(XB1, XB2, s, 0.0D0, -t44, 0.0D0, t42, 0.0D0, t92)
      t95 = -0.1D1 + x1
      t97 = x1 * z
      t98 = 0.1D1 - x1 + t97
      t99 = 0.1D1 / t98
      t101 = t2 * t95 * x2 * t99
      t102 = t2 * x1
      t104 = t43 * t1 * t95
      t109 = s * t19 * x2 * t95 * x1 * t99
      t111 = x2 * x1
      t114 = (-0.1D1 + x2 - t111 - t22 + t111 * z - t97 + x1) ** 2
      t117 = t95 * t98 / t114 * t68
      t118 = t21 * t117
      t120 = FJET(XB1, XB2, s, 0.0D0, -t101, t102, t104, -t109, 0.6D1 * 
     #t118)
      t122 = t19 * wd
      t127 = FJET(XB1, XB2, s, t102, t104, 0.0D0, -t101, -t109, 0.4D1 / 
     #0.3D1 * t118)
      gbgbH8n1em1 = -t33 * z * t19 * t36 * t27 * t29 / 0.3D1 + t73 * t72
     # - t80 * z * t19 * t36 * t75 * t29 / 0.3D1 + t93 * t92 + 0.6D1 * t
     #120 * z * t122 * t117 + 0.4D1 / 0.3D1 * t127 * z * t122 * t117

      end function



      doubleprecision function gbgbH8n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t2 = -0.1D1 + z
      t3 = x2 * s * t2
      t6 = (-0.1D1 + x2) * s * t2
      t7 = t2 ** 2
      t11 = (0.1D1 + x2 * z - x2) ** 2
      t12 = 0.1D1 / t11
      t14 = z * t7 * wd * t12
      t16 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t6, 0.0D0, 0.3D1 * t14)
      t19 = t7 * wd * t12
      t23 = FJET(XB1, XB2, s, 0.0D0, -t6, 0.0D0, t3, 0.0D0, 0.2D1 / 0.3D
     #1 * t14)
      gbgbH8n1em2 = 0.3D1 * t16 * z * t19 + 0.2D1 / 0.3D1 * t23 * z * t1
     #9

      end function



      doubleprecision function gbgbH8n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      gbgbH8n1em3 = 0.0D0

      end function



      doubleprecision function gbgbH8n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      gbgbH8n1em4 = 0.0D0

      end function
