  
      subroutine bbbbH7n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision bbbbH7n1e1  
      doubleprecision bbbbH7n1e0  
      doubleprecision bbbbH7n1em1  
      doubleprecision bbbbH7n1em2  
      doubleprecision bbbbH7n1em3  
      doubleprecision bbbbH7n1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=bbbbH7n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=bbbbH7n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=bbbbH7n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=bbbbH7n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=bbbbH7n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=bbbbH7n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function bbbbH7n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = t2 * x2
      t4 = -0.1D1 + x2
      t5 = t2 * t4
      t6 = z ** 2
      t7 = 0.1D1 / t6
      t8 = t7 * x2
      t9 = x4 * 0.3141592653589793D1
      t10 = Sin(t9)
      t11 = t10 ** 2
      t12 = t4 * t11
      t15 = log(-0.4D1 * t8 * t12)
      t17 = t15 * x2 * z
      t18 = t1 ** 2
      t19 = t18 * t1
      t20 = wd * t19
      t21 = t20 * lh
      t24 = x2 * z
      t25 = lh ** 2
      t27 = 0.3141592653589793D1 ** 2
      t29 = 0.180D3 * t25 - 0.30D2 * t27
      t30 = t20 * t29
      t31 = t24 * t30
      t32 = t15 ** 2
      t33 = t32 * x2
      t35 = z * wd * t19
      t57 = (0.1D1 + t24 - x2) ** 2
      t58 = 0.1D1 / t57
      t63 = t24 * t21
      t66 = (-0.90D2 * t24 * t20 - 0.720D3 * t63) * t58
      t68 = x2 * t4
      t72 = log(-0.4D1 * x3 * t7 * t68 * t11)
      t74 = t24 * wd
      t75 = t19 * t58
      t76 = t72 ** 2
      t83 = (0.180D3 * t63 + 0.4D1 * t31) * t58
      t85 = 0.1D1 / x3
      t88 = x2 * wd
      t89 = x1 ** 2
      t90 = x3 * t89
      t92 = t12 * t7
      t95 = log(-0.4D1 * t90 * x2 * t92)
      t103 = t88 * t19
      t107 = 0.720D3 * t103 * lh * z * t58
      t110 = 0.1D1 / x1
      t113 = x2 * t89
      t116 = log(-0.4D1 * t113 * t92)
      t117 = t116 * z
      t124 = t116 ** 2
      t139 = -(-0.180D3 * t17 * t21 - t31 - 0.45D2 * t33 * t35 - 0.4D1 *
     # t17 * t30 - 0.360D3 * t33 * z * t21 + 0.4D1 * t24 * t20 * (0.60D2
     # * lh * t27 - 0.2884936567583026D3 - 0.120D3 * t25 * lh) - 0.60D2 
     #* t32 * t15 * x2 * t35) * t58 / 0.540D3 - (-t66 * t72 + 0.180D3 * 
     #t74 * t75 * t76 + t83) * t85 / 0.540D3 + (-0.90D2 * t88 * t19 * (-
     #z - 0.4D1 * t95 * z) * t58 + t107) * t85 * t110 / 0.270D3 + (0.180
     #D3 * t103 * lh * (-z - 0.4D1 * t117) * t58 - 0.90D2 * t88 * t19 * 
     #(t117 + 0.2D1 * t124 * z) * t58 - 0.4D1 * t103 * t29 * z * t58) * 
     #t110 / 0.270D3
      t140 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t139)
      t142 = x2 * x3
      t143 = 0.2D1 * t142
      t144 = cos(t9)
      t145 = -0.1D1 + x3
      t147 = t68 * x3 * t145
      t148 = Sqrt(t147)
      t150 = 0.2D1 * t144 * t148
      t152 = t2 * (0.1D1 - x2 - x3 + t143 + t150)
      t154 = t2 * (-x3 + t143 - x2 + t150)
      t155 = t11 * t7
      t157 = t4 * x3
      t161 = log(0.4D1 * t155 * x2 * t157 * t145)
      t163 = t161 ** 2
      t170 = t89 * t11
      t174 = log(0.4D1 * t170 * t7 * t147)
      t186 = -(t66 * t161 - 0.180D3 * t74 * t75 * t163 - t83) * t85 / 0.
     #540D3 + (0.90D2 * t88 * t19 * (-z - 0.4D1 * t174 * z) * t58 - t107
     #) * t85 * t110 / 0.270D3
      t187 = FJET(XB1, XB2, s, 0.0D0, t152, 0.0D0, -t154, 0.0D0, t186)
      t189 = FJET(XB1, XB2, s, 0.0D0, -t5, 0.0D0, t3, 0.0D0, t139)
      t191 = FJET(XB1, XB2, s, 0.0D0, -t154, 0.0D0, t152, 0.0D0, t186)
      t193 = -0.1D1 + x1
      t195 = x1 * z
      t196 = 0.1D1 - x1 + t195
      t197 = 0.1D1 / t196
      t199 = t2 * t193 * x2 * t197
      t200 = t2 * x1
      t203 = t4 * s * t1 * t193
      t208 = s * t18 * x2 * x1 * t193 * t197
      t209 = t193 ** 2
      t210 = t197 * t209
      t211 = t89 * t6
      t213 = 0.2D1 * t89 * z
      t215 = x1 * t6
      t216 = x2 * x1
      t218 = 0.2D1 * t113 * z
      t219 = t216 * t6
      t220 = t216 * z
      t221 = 0.2D1 * t220
      t222 = t113 * t6
      t223 = t211 - t213 + t89 - t113 - x1 + 0.2D1 * t195 - z - t215 + t
     #216 + t218 + t219 - t221 - t222
      t224 = t210 * t223
      t226 = t155 * t210
      t229 = log(-0.4D1 * t90 * t68 * t226)
      t234 = 0.4D1 * z - t216 - t211 + x1 + t113 + t213 - t219 - t218 + 
     #t221 - t89 + 0.4D1 * t215 - 0.5D1 * t195 + t222
      t235 = t197 * t234
      t240 = (-0.1D1 - t24 - t216 + x1 + x2 - t195 + t220) ** 2
      t241 = 0.1D1 / t240
      t248 = t210 * t234 * t241
      t250 = 0.180D3 * t88 * t19 * lh * t248
      t257 = log(-0.4D1 * t113 * t4 * t226)
      t258 = t257 * t209
      t267 = t257 ** 2
      t282 = (0.90D2 * t88 * t19 * (t224 - t229 * t209 * t235) * t241 - 
     #t250) * t85 * t110 / 0.270D3 + (-0.180D3 * t103 * lh * (t224 - t25
     #8 * t235) * t241 + 0.90D2 * t88 * t19 * (-t258 * t197 * t223 + t26
     #7 * t209 * t235 / 0.2D1) * t241 + t88 * t19 * t29 * t248) * t110 /
     # 0.270D3
      t283 = FJET(XB1, XB2, s, 0.0D0, -t199, t200, t203, -t208, t282)
      t285 = FJET(XB1, XB2, s, t200, t203, 0.0D0, -t199, -t208, t282)
      t287 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t139)
      t289 = FJET(XB1, XB2, s, t152, 0.0D0, -t154, 0.0D0, 0.0D0, t186)
      t291 = x3 * x1
      t292 = t2 * t291
      t293 = t291 * z
      t294 = t142 * x1
      t295 = t142 * t195
      t299 = Sqrt(t157 * t196 * x2 * t145)
      t301 = 0.2D1 * t144 * t299
      t305 = t2 * t193 * (-x3 + t291 - t293 + t143 - t294 + t295 - x2 + 
     #t301) * t197
      t307 = t2 * x1 * t145
      t308 = 0.1D1 - x1 + t195 - x2 + t216 - t220 - x3 + t291 - t293 + t
     #143 - t294 + t295 + t301
      t311 = t2 * t193 * t308 * t197
      t318 = log(0.4D1 * t170 * t8 * t157 * t145 * t197 * t209)
      t326 = -0.90D2 * t88 * t19 * (t224 - t318 * t209 * t235) * t241 + 
     #t250
      t329 = t326 * t85 * t110 / 0.270D3
      t330 = FJET(XB1, XB2, s, t292, t305, -t307, -t311, -t208, t329)
      t332 = t85 * t110
      t335 = FJET(XB1, XB2, s, t203, t200, -t199, 0.0D0, -t208, t282)
      t337 = FJET(XB1, XB2, s, t305, t292, -t311, -t307, -t208, t329)
      t341 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, t139)
      t343 = FJET(XB1, XB2, s, -t154, 0.0D0, t152, 0.0D0, 0.0D0, t186)
      t345 = FJET(XB1, XB2, s, -t307, -t311, t292, t305, -t208, t329)
      t349 = FJET(XB1, XB2, s, -t199, 0.0D0, t203, t200, -t208, t282)
      t351 = FJET(XB1, XB2, s, -t311, -t307, t305, t292, -t208, t329)
      bbbbH7n1e1 = t140 * t139 + t187 * t186 + t189 * t139 + t191 * t186
     # + t283 * t282 + t285 * t282 + t287 * t139 + t289 * t186 + t330 * 
     #t326 * t332 / 0.270D3 + t335 * t282 + t337 * t326 * t332 / 0.270D3
     # + t341 * t139 + t343 * t186 + t345 * t326 * t332 / 0.270D3 + t349
     # * t282 + t351 * t326 * t332 / 0.270D3

      end function



      doubleprecision function bbbbH7n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = t2 * x2
      t4 = -0.1D1 + x2
      t5 = t2 * t4
      t6 = x2 * z
      t7 = t6 * wd
      t8 = t1 ** 2
      t9 = t8 * t1
      t11 = (0.1D1 + t6 - x2) ** 2
      t12 = 0.1D1 / t11
      t13 = t9 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t17 = x2 * t4
      t18 = x4 * 0.3141592653589793D1
      t19 = Sin(t18)
      t20 = t19 ** 2
      t24 = log(-0.4D1 * x3 * t15 * t17 * t20)
      t28 = wd * t9
      t31 = t28 * lh
      t32 = t6 * t31
      t35 = (-0.90D2 * t6 * t28 - 0.720D3 * t32) * t12
      t37 = 0.1D1 / x3
      t40 = 0.1D1 / x1
      t44 = 0.4D1 / 0.3D1 * t7 * t13 * t37 * t40
      t45 = x2 * wd
      t46 = x1 ** 2
      t47 = x2 * t46
      t48 = t4 * t20
      t52 = log(-0.4D1 * t47 * t48 * t15)
      t72 = log(-0.4D1 * t15 * x2 * t48)
      t73 = t72 * x2
      t75 = z * wd * t9
      t81 = lh ** 2
      t83 = 0.3141592653589793D1 ** 2
      t89 = t72 ** 2
      t96 = -(-0.360D3 * t7 * t13 * t24 + t35) * t37 / 0.540D3 - t44 + (
     #-0.90D2 * t45 * t9 * (-z - 0.4D1 * t52 * z) * t12 + 0.720D3 * t45 
     #* t9 * lh * z * t12) * t40 / 0.270D3 - (0.180D3 * t32 + 0.90D2 * t
     #73 * t75 + 0.720D3 * t73 * z * t31 + 0.4D1 * t6 * t28 * (0.180D3 *
     # t81 - 0.30D2 * t83) + 0.180D3 * t89 * x2 * t75) * t12 / 0.540D3
      t97 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t96)
      t99 = x2 * x3
      t100 = 0.2D1 * t99
      t101 = cos(t18)
      t102 = -0.1D1 + x3
      t105 = Sqrt(t17 * x3 * t102)
      t107 = 0.2D1 * t101 * t105
      t109 = t2 * (0.1D1 - x2 - x3 + t100 + t107)
      t111 = t2 * (-x3 + t100 - x2 + t107)
      t112 = t20 * t15
      t114 = t4 * x3
      t118 = log(0.4D1 * t112 * x2 * t114 * t102)
      t125 = -(0.360D3 * t7 * t13 * t118 - t35) * t37 / 0.540D3 + t44
      t126 = FJET(XB1, XB2, s, 0.0D0, t109, 0.0D0, -t111, 0.0D0, t125)
      t128 = FJET(XB1, XB2, s, 0.0D0, -t5, 0.0D0, t3, 0.0D0, t96)
      t130 = FJET(XB1, XB2, s, 0.0D0, -t111, 0.0D0, t109, 0.0D0, t125)
      t132 = -0.1D1 + x1
      t134 = x1 * z
      t135 = 0.1D1 - x1 + t134
      t136 = 0.1D1 / t135
      t138 = t2 * t132 * x2 * t136
      t139 = t2 * x1
      t142 = t4 * s * t1 * t132
      t147 = s * t8 * x2 * x1 * t132 * t136
      t150 = t132 ** 2
      t152 = x2 * x1
      t153 = t46 * t14
      t155 = 0.2D1 * t46 * z
      t156 = t152 * t14
      t158 = 0.2D1 * t47 * z
      t159 = t152 * z
      t160 = 0.2D1 * t159
      t161 = x1 * t14
      t164 = t47 * t14
      t165 = 0.4D1 * z - t152 - t153 + x1 + t47 + t155 - t156 - t158 + t
     #160 - t46 + 0.4D1 * t161 - 0.5D1 * t134 + t164
      t168 = (-0.1D1 - t6 - t152 + x1 + x2 - t134 + t159) ** 2
      t169 = 0.1D1 / t168
      t172 = t150 * t165 * t169 * t37 * t40
      t174 = t45 * t9 * t136 * t172 / 0.3D1
      t175 = t136 * t150
      t177 = t153 - t155 + t46 - t47 - x1 + 0.2D1 * t134 - z - t161 + t1
     #52 + t158 + t156 - t160 - t164
      t183 = log(-0.4D1 * t47 * t4 * t112 * t175)
      t201 = t174 + (0.90D2 * t45 * t9 * (t175 * t177 - t183 * t150 * t1
     #36 * t165) * t169 - 0.180D3 * t45 * t9 * lh * t175 * t165 * t169) 
     #* t40 / 0.270D3
      t202 = FJET(XB1, XB2, s, 0.0D0, -t138, t139, t142, -t147, t201)
      t204 = FJET(XB1, XB2, s, t139, t142, 0.0D0, -t138, -t147, t201)
      t206 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t96)
      t208 = FJET(XB1, XB2, s, t109, 0.0D0, -t111, 0.0D0, 0.0D0, t125)
      t210 = x3 * x1
      t211 = t2 * t210
      t212 = t210 * z
      t213 = t99 * x1
      t214 = t99 * t134
      t218 = Sqrt(t114 * t135 * x2 * t102)
      t220 = 0.2D1 * t101 * t218
      t224 = t2 * t132 * (-x3 + t210 - t212 + t100 - t213 + t214 - x2 + 
     #t220) * t136
      t226 = t2 * x1 * t102
      t227 = 0.1D1 - x1 + t134 - x2 + t152 - t159 - x3 + t210 - t212 + t
     #100 - t213 + t214 + t220
      t230 = t2 * t132 * t227 * t136
      t231 = FJET(XB1, XB2, s, t211, t224, -t226, -t230, -t147, -t174)
      t233 = t28 * t136
      t237 = FJET(XB1, XB2, s, t142, t139, -t138, 0.0D0, -t147, t201)
      t239 = FJET(XB1, XB2, s, t224, t211, -t230, -t226, -t147, -t174)
      t244 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, t96)
      t246 = FJET(XB1, XB2, s, -t111, 0.0D0, t109, 0.0D0, 0.0D0, t125)
      t248 = FJET(XB1, XB2, s, -t226, -t230, t211, t224, -t147, -t174)
      t253 = FJET(XB1, XB2, s, -t138, 0.0D0, t142, t139, -t147, t201)
      t255 = FJET(XB1, XB2, s, -t230, -t226, t224, t211, -t147, -t174)
      bbbbH7n1e0 = t97 * t96 + t126 * t125 + t128 * t96 + t130 * t125 + 
     #t202 * t201 + t204 * t201 + t206 * t96 + t208 * t125 - t231 * x2 *
     # t233 * t172 / 0.3D1 + t237 * t201 - t239 * x2 * t233 * t172 / 0.3
     #D1 + t244 * t96 + t246 * t125 - t248 * x2 * t233 * t172 / 0.3D1 + 
     #t253 * t201 - t255 * x2 * t233 * t172 / 0.3D1

      end function



      doubleprecision function bbbbH7n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = t2 * x2
      t4 = -0.1D1 + x2
      t5 = t2 * t4
      t6 = x2 * z
      t7 = t1 ** 2
      t8 = t7 * t1
      t9 = wd * t8
      t15 = z ** 2
      t18 = x4 * 0.3141592653589793D1
      t19 = Sin(t18)
      t20 = t19 ** 2
      t24 = log(-0.4D1 / t15 * x2 * t4 * t20)
      t32 = (0.1D1 + t6 - x2) ** 2
      t33 = 0.1D1 / t32
      t36 = t6 * wd
      t37 = t8 * t33
      t38 = 0.1D1 / x3
      t41 = 0.2D1 / 0.3D1 * t36 * t37 * t38
      t42 = 0.1D1 / x1
      t46 = -(-0.90D2 * t6 * t9 - 0.720D3 * t6 * t9 * lh - 0.360D3 * t24
     # * x2 * z * wd * t8) * t33 / 0.540D3 - t41 - 0.4D1 / 0.3D1 * t36 *
     # t37 * t42
      t47 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t46)
      t50 = 0.2D1 * x2 * x3
      t51 = cos(t18)
      t56 = Sqrt(x2 * t4 * x3 * (-0.1D1 + x3))
      t58 = 0.2D1 * t51 * t56
      t60 = t2 * (0.1D1 - x2 - x3 + t50 + t58)
      t62 = t2 * (-x3 + t50 - x2 + t58)
      t63 = FJET(XB1, XB2, s, 0.0D0, t60, 0.0D0, -t62, 0.0D0, t41)
      t67 = t9 * t33 * t38
      t70 = FJET(XB1, XB2, s, 0.0D0, -t5, 0.0D0, t3, 0.0D0, t46)
      t72 = FJET(XB1, XB2, s, 0.0D0, -t62, 0.0D0, t60, 0.0D0, t41)
      t77 = -0.1D1 + x1
      t79 = x1 * z
      t81 = 0.1D1 / (0.1D1 - x1 + t79)
      t83 = t2 * t77 * x2 * t81
      t84 = t2 * x1
      t87 = t4 * s * t1 * t77
      t92 = s * t7 * x2 * x1 * t77 * t81
      t96 = t77 ** 2
      t98 = x2 * x1
      t99 = x1 ** 2
      t101 = x2 * t99
      t107 = t98 * z
      t113 = 0.4D1 * z - t98 - t99 * t15 + x1 + t101 + 0.2D1 * t99 * z -
     # t98 * t15 - 0.2D1 * t101 * z + 0.2D1 * t107 - t99 + 0.4D1 * x1 * 
     #t15 - 0.5D1 * t79 + t101 * t15
      t116 = (-0.1D1 - t6 - t98 + x1 + x2 - t79 + t107) ** 2
      t117 = 0.1D1 / t116
      t121 = x2 * wd * t8 * t81 * t96 * t113 * t117 * t42 / 0.3D1
      t122 = FJET(XB1, XB2, s, 0.0D0, -t83, t84, t87, -t92, t121)
      t128 = t81 * t96 * t113 * t117 * t42
      t131 = FJET(XB1, XB2, s, t84, t87, 0.0D0, -t83, -t92, t121)
      t136 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t46)
      t138 = FJET(XB1, XB2, s, t60, 0.0D0, -t62, 0.0D0, 0.0D0, t41)
      t143 = FJET(XB1, XB2, s, t87, t84, -t83, 0.0D0, -t92, t121)
      t148 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, t46)
      t150 = FJET(XB1, XB2, s, -t62, 0.0D0, t60, 0.0D0, 0.0D0, t41)
      t155 = FJET(XB1, XB2, s, -t83, 0.0D0, t87, t84, -t92, t121)
      bbbbH7n1em1 = t47 * t46 + 0.2D1 / 0.3D1 * t63 * x2 * z * t67 + t70
     # * t46 + 0.2D1 / 0.3D1 * t72 * x2 * z * t67 + t122 * x2 * t9 * t12
     #8 / 0.3D1 + t131 * x2 * t9 * t128 / 0.3D1 + t136 * t46 + 0.2D1 / 0
     #.3D1 * t138 * x2 * z * t67 + t143 * x2 * t9 * t128 / 0.3D1 + t148 
     #* t46 + 0.2D1 / 0.3D1 * t150 * x2 * z * t67 + t155 * x2 * t9 * t12
     #8 / 0.3D1

      end function



      doubleprecision function bbbbH7n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = t2 * x2
      t5 = t2 * (-0.1D1 + x2)
      t6 = x2 * z
      t7 = t1 ** 2
      t11 = (0.1D1 + t6 - x2) ** 2
      t13 = wd * t7 * t1 / t11
      t15 = 0.2D1 / 0.3D1 * t6 * t13
      t16 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, -t15)
      t20 = FJET(XB1, XB2, s, 0.0D0, -t5, 0.0D0, t3, 0.0D0, -t15)
      t24 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, -t15)
      t28 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, -t15)
      bbbbH7n1em2 = -0.2D1 / 0.3D1 * t16 * x2 * z * t13 - 0.2D1 / 0.3D1 
     #* t20 * x2 * z * t13 - 0.2D1 / 0.3D1 * t24 * x2 * z * t13 - 0.2D1 
     #/ 0.3D1 * t28 * x2 * z * t13

      end function



      doubleprecision function bbbbH7n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      bbbbH7n1em3 = 0.0D0

      end function



      doubleprecision function bbbbH7n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      bbbbH7n1em4 = 0.0D0

      end function
