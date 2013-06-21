  
      subroutine bbggh9n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision bbggh9n1e1  
      doubleprecision bbggh9n1e0  
      doubleprecision bbggh9n1em1  
      doubleprecision bbggh9n1em2  
      doubleprecision bbggh9n1em3  
      doubleprecision bbggh9n1em4  
      doubleprecision bbggh9n2e1  
      doubleprecision bbggh9n2e0  
      doubleprecision bbggh9n2em1  
      doubleprecision bbggh9n2em2  
      doubleprecision bbggh9n2em3  
      doubleprecision bbggh9n2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=bbggh9n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbggh9n2e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=bbggh9n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbggh9n2e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=bbggh9n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbggh9n2em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=bbggh9n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbggh9n2em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=bbggh9n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbggh9n2em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=bbggh9n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbggh9n2em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function bbggh9n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t6 = x4 * 0.3141592653589793D1
      t7 = Sin(t6)
      t8 = t7 ** 2
      t10 = z ** 2
      t11 = 0.1D1 / t10
      t15 = log(-0.4D1 * x3 * t8 * t11 * t4)
      t18 = lh ** 2
      t19 = 0.180D3 * t18
      t20 = 0.3141592653589793D1 ** 2
      t21 = 0.30D2 * t20
      t22 = t15 ** 2
      t26 = -t19 + t21
      t40 = cos(t6)
      t41 = t40 ** 2
      t42 = x3 * t4
      t43 = Sqrt(-t42)
      t44 = t43 ** 2
      t45 = t41 * t44
      t50 = 0.180D3 * z * lh
      t52 = (0.90D2 * z + t50) * wd
      t53 = x2 * x3
      t54 = t8 * t11
      t55 = t54 * t4
      t58 = log(-0.4D1 * t53 * t55)
      t63 = z * wd
      t64 = t58 ** 2
      t71 = (-t50 + t26 * z) * wd
      t72 = t71 * t45
      t75 = 0.1D1 / x2
      t78 = x1 ** 2
      t82 = log(-0.4D1 * t53 * t78 * t55)
      t91 = 0.1D1 / x1
      t94 = x3 * t78
      t97 = log(-0.4D1 * t94 * t55)
      t101 = t97 ** 2
      t109 = -0.2D1 / 0.15D2 * (-(-0.180D3 * t15 * lh - t19 + t21 - 0.45
     #D2 * t22) * z + (-t15 * t26 + 0.90D2 * t22 * lh - 0.60D2 * lh * t2
     #0 + 0.2884936567583026D3 + 0.120D3 * t18 * lh + 0.15D2 * t22 * t15
     #) * z) * wd * t45 - (-0.4D1 * t52 * t58 * t41 * t44 - 0.180D3 * t6
     #3 * t64 * t41 * t44 + 0.4D1 * t72) * t75 / 0.30D2 + (-0.360D3 * t6
     #3 * t82 * t41 * t44 - 0.4D1 * t52 * t45) * t75 * t91 / 0.15D2 + 0.
     #4D1 / 0.15D2 * (t52 * t41 * t97 * t44 + 0.45D2 * t63 * t41 * t101 
     #* t44 - t72) * t91
      t110 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t
     #109)
      t112 = -0.1D1 + x1
      t113 = x1 * x3
      t114 = t113 * z
      t115 = 0.2D1 * t53
      t116 = t53 * x1
      t117 = x1 * z
      t118 = t53 * t117
      t119 = sqrt(x2)
      t120 = t40 * t119
      t121 = -0.1D1 + t119
      t122 = x3 * t121
      t123 = t119 + 0.1D1
      t124 = 0.1D1 - x1 + t117
      t128 = Sqrt(t122 * t123 * t124 * t4)
      t130 = 0.2D1 * t120 * t128
      t133 = 0.1D1 / t124
      t136 = t2 * t113
      t137 = x2 * x1
      t138 = t137 * z
      t139 = 0.1D1 - x1 + t117 - x2 + t137 - t138 - x3 + t113 - t114 + t
     #115 - t116 + t118 + t130
      t143 = t4 * s
      t145 = t143 * t1 * x1
      t146 = t1 ** 2
      t152 = t112 ** 2
      t162 = log(0.4D1 * t152 * x3 * t4 * t121 * t123 * t54 * t133 * x2 
     #* t78)
      t164 = x2 * z
      t166 = (-0.1D1 + x1 - t117 + x2 - t137 - t164 + t138) ** 2
      t169 = t119 * x1
      t171 = t119 * x2
      t172 = t171 * x1
      t174 = t78 * t171
      t175 = x3 * t119
      t176 = 0.3D1 * t175
      t177 = t119 * z
      t178 = t171 * x3
      t179 = 0.2D1 * t178
      t180 = t171 * z
      t181 = t119 * t78
      t183 = t40 * t128
      t188 = t10 * x3
      t196 = t128 * x1
      t199 = x2 * t40
      t208 = -0.4D1 * t169 + 0.2D1 * t172 - t174 - t176 - t177 + t179 + 
     #t180 + 0.2D1 * t181 - 0.2D1 * t183 - 0.2D1 * t174 * x3 * z - t172 
     #* t188 + t174 * t188 - 0.2D1 * t164 * t183 + 0.4D1 * t178 * t117 -
     # 0.2D1 * z * t40 * t196 - 0.2D1 * t199 * t196 - 0.2D1 * t94 * t10 
     #* t119 + 0.4D1 * t94 * t177 + t188 * t169
      t211 = 0.2D1 * t119
      t214 = t183 * x1
      t236 = t175 * z
      t238 = 0.2D1 * t178 * z
      t241 = -0.6D1 * t175 * t117 - t171 + t211 - 0.2D1 * t94 * t119 + 0
     #.2D1 * t214 + 0.2D1 * t199 * t128 + 0.5D1 * t175 * x1 - t169 * t10
     # - 0.4D1 * t181 * z + 0.2D1 * t181 * t10 - t174 * t10 + t174 * x3 
     #- 0.3D1 * t172 * x3 - 0.3D1 * t172 * z + 0.2D1 * t174 * z + t172 *
     # t10 + 0.5D1 * t169 * z + t236 - t238 + 0.2D1 * t164 * t214
      t243 = (t208 + t241) ** 2
      t244 = t133 / t166 * t243
      t248 = -0.90D2 * t63 * t162 * t244 - t52 * t244
      t252 = FJET(XB1, XB2, s, t2 * t112 * (-x3 + t113 - t114 + t115 - t
     #116 + t118 - x2 + t130) * t133, t136, -t2 * t112 * t139 * t133, -t
     #145, -s * t146 * x2 * x1 * t112 * t133, t248 * t75 * t91 / 0.15D2)
      t259 = Sqrt(t122 * t123 * t4)
      t261 = 0.2D1 * t120 * t259
      t268 = t54 * t121 * t123
      t271 = log(0.4D1 * t53 * t4 * t268)
      t272 = t40 * t259
      t278 = -t236 + 0.2D1 * t272 - 0.2D1 * t199 * t259 + t238 + t176 + 
     #t177 - t179 - t180 + t171 - t211 + 0.2D1 * t164 * t272
      t279 = t278 ** 2
      t282 = (0.1D1 - x2 + t164) ** 2
      t283 = 0.1D1 / t282
      t286 = t271 ** 2
      t291 = t279 * t283
      t300 = log(0.4D1 * x2 * t78 * t42 * t268)
      t310 = -(t52 * t271 * t279 * t283 + 0.45D2 * t63 * t286 * t279 * t
     #283 - t71 * t291) * t75 / 0.30D2 + (0.90D2 * t63 * t300 * t279 * t
     #283 + t52 * t291) * t75 * t91 / 0.15D2
      t311 = FJET(XB1, XB2, s, -t2 * (-x3 + t115 - x2 + t261), 0.0D0, t2
     # * (0.1D1 - x2 - x3 + t115 + t261), 0.0D0, 0.0D0, t310)
      t321 = t11 * t133 * t152 * t4
      t324 = log(-0.4D1 * t53 * t78 * t8 * t321)
      t328 = Sqrt(-x3 * t124 * t4)
      t329 = t328 ** 2
      t331 = t133 * t329 * t41
      t344 = log(-0.4D1 * t94 * t8 * t321)
      t349 = t344 ** 2
      t358 = (0.360D3 * t63 * t324 * t331 + 0.4D1 * t52 * t331) * t75 * 
     #t91 / 0.15D2 + 0.4D1 / 0.15D2 * (-t52 * t41 * t344 * t133 * t329 -
     # 0.45D2 * t63 * t41 * t349 * t133 * t329 + t71 * t331) * t91
      t359 = FJET(XB1, XB2, s, -t2 * t112 * x3, t136, t143 * t1 * t112, 
     #-t145, 0.0D0, t358)
      bbggh9n1e1 = t110 * t109 + t252 * t248 * t75 * t91 / 0.15D2 + t311
     # * t310 + t359 * t358

      end function



      doubleprecision function bbggh9n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
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
      t18 = cos(t8)
      t19 = t18 ** 2
      t22 = Sqrt(-x3 * t4)
      t23 = t22 ** 2
      t31 = (0.90D2 * z + 0.180D3 * z * lh) * wd
      t32 = t19 * t23
      t33 = t31 * t32
      t36 = 0.1D1 / x2
      t39 = t6 * t19
      t41 = 0.1D1 / x1
      t45 = x1 ** 2
      t46 = x3 * t45
      t49 = log(-0.4D1 * t46 * t14)
      t62 = log(-0.4D1 * x3 * t10 * t12 * t4)
      t68 = lh ** 2
      t70 = 0.3141592653589793D1 ** 2
      t72 = t62 ** 2
      t80 = -(0.360D3 * t6 * t17 * t19 * t23 + 0.4D1 * t33) * t36 / 0.30
     #D2 + 0.24D2 * t39 * t23 * t36 * t41 + 0.4D1 / 0.15D2 * (-0.90D2 * 
     #t6 * t19 * t49 * t23 - t33) * t41 - 0.2D1 / 0.15D2 * (-(0.180D3 * 
     #lh + 0.90D2 * t62) * z + (-0.180D3 * t62 * lh - 0.180D3 * t68 + 0.
     #30D2 * t70 - 0.45D2 * t72) * z) * wd * t32
      t81 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t8
     #0)
      t83 = -0.1D1 + x1
      t84 = x1 * x3
      t85 = t84 * z
      t86 = 0.2D1 * t7
      t87 = t7 * x1
      t88 = x1 * z
      t89 = t7 * t88
      t90 = sqrt(x2)
      t91 = t18 * t90
      t92 = -0.1D1 + t90
      t93 = x3 * t92
      t94 = t90 + 0.1D1
      t95 = 0.1D1 - x1 + t88
      t99 = Sqrt(t93 * t94 * t95 * t4)
      t101 = 0.2D1 * t91 * t99
      t104 = 0.1D1 / t95
      t107 = t2 * t84
      t108 = x2 * x1
      t109 = t108 * z
      t110 = 0.1D1 - x1 + t88 - x2 + t108 - t109 - x3 + t84 - t85 + t86 
     #- t87 + t89 + t101
      t114 = t4 * s
      t116 = t114 * t1 * x1
      t117 = t1 ** 2
      t123 = t6 * t104
      t124 = x2 * z
      t126 = (-0.1D1 + x1 - t88 + x2 - t108 - t124 + t109) ** 2
      t130 = t18 * t99
      t131 = t130 * x1
      t133 = x2 * t18
      t136 = x3 * t90
      t139 = t90 * x2
      t140 = t90 * x1
      t142 = t90 * t45
      t147 = t45 * t139
      t150 = x1 * t139
      t160 = t139 * x3
      t164 = t99 * x1
      t172 = -0.2D1 * t46 * t90 + 0.2D1 * t131 + 0.2D1 * t133 * t99 + 0.
     #5D1 * t136 * x1 - t139 - t140 * t11 - 0.4D1 * t142 * z + 0.2D1 * t
     #142 * t11 - t147 * t11 + t147 * x3 - 0.3D1 * t150 * x3 - 0.3D1 * t
     #150 * z + 0.2D1 * t147 * z + t150 * t11 - 0.2D1 * t124 * t130 + 0.
     #4D1 * t160 * t88 - 0.2D1 * z * t18 * t164 - 0.2D1 * t133 * t164 - 
     #0.2D1 * t46 * t11 * t90
      t173 = t90 * z
      t176 = x3 * t11
      t187 = t136 * z
      t189 = 0.2D1 * t160 * z
      t192 = 0.3D1 * t136
      t193 = 0.2D1 * t160
      t194 = t139 * z
      t197 = 0.2D1 * t90
      t200 = 0.4D1 * t46 * t173 + t176 * t140 - 0.6D1 * t136 * t88 - 0.2
     #D1 * t147 * x3 * z - t150 * t176 + t147 * t176 + 0.5D1 * t140 * z 
     #+ t187 - t189 - 0.4D1 * t140 + 0.2D1 * t150 - t147 - t192 - t173 +
     # t193 + t194 + 0.2D1 * t142 - 0.2D1 * t130 + t197 + 0.2D1 * t124 *
     # t131
      t202 = (t172 + t200) ** 2
      t204 = t36 * t41
      t205 = 0.1D1 / t126 * t202 * t204
      t208 = FJET(XB1, XB2, s, t2 * t83 * (-x3 + t84 - t85 + t86 - t87 +
     # t89 - x2 + t101) * t104, t107, -t2 * t83 * t110 * t104, -t116, -s
     # * t117 * x2 * x1 * t83 * t104, 0.6D1 * t123 * t205)
      t216 = Sqrt(t93 * t94 * t4)
      t218 = 0.2D1 * t91 * t216
      t228 = log(0.4D1 * t7 * t4 * t13 * t92 * t94)
      t229 = t18 * t216
      t235 = -t187 + 0.2D1 * t229 - 0.2D1 * t133 * t216 + t189 + t192 + 
     #t173 - t193 - t194 + t139 - t197 + 0.2D1 * t124 * t229
      t236 = t235 ** 2
      t239 = (0.1D1 - x2 + t124) ** 2
      t240 = 0.1D1 / t239
      t254 = -(-0.90D2 * t6 * t228 * t236 * t240 - t31 * t236 * t240) * 
     #t36 / 0.30D2 - 0.6D1 * t6 * t236 * t240 * t36 * t41
      t255 = FJET(XB1, XB2, s, -t2 * (-x3 + t86 - x2 + t218), 0.0D0, t2 
     #* (0.1D1 - x2 - x3 + t86 + t218), 0.0D0, 0.0D0, t254)
      t263 = Sqrt(-x3 * t95 * t4)
      t264 = t263 ** 2
      t271 = t83 ** 2
      t276 = log(-0.4D1 * t46 * t10 * t12 * t104 * t271 * t4)
      t287 = -0.24D2 * t123 * t264 * t19 * t204 + 0.4D1 / 0.15D2 * (0.90
     #D2 * t39 * t276 * t104 * t264 + t31 * t104 * t264 * t19) * t41
      t288 = FJET(XB1, XB2, s, -t2 * t83 * x3, t107, t114 * t1 * t83, -t
     #116, 0.0D0, t287)
      bbggh9n1e0 = t81 * t80 + 0.6D1 * t208 * z * wd * t104 * t205 + t25
     #5 * t254 + t288 * t287

      end function



      doubleprecision function bbggh9n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t8 = x4 * 0.3141592653589793D1
      t9 = Sin(t8)
      t10 = t9 ** 2
      t12 = z ** 2
      t17 = log(-0.4D1 * x3 * t10 / t12 * t4)
      t23 = cos(t8)
      t24 = t23 ** 2
      t26 = Sqrt(-x3 * t4)
      t27 = t26 ** 2
      t28 = t24 * t27
      t31 = z * wd
      t32 = 0.1D1 / x2
      t36 = 0.1D1 / x1
      t40 = -0.2D1 / 0.15D2 * (0.90D2 * z + (0.180D3 * lh + 0.90D2 * t17
     #) * z) * wd * t28 + 0.12D2 * t31 * t28 * t32 + 0.24D2 * t31 * t28 
     #* t36
      t41 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t4
     #0)
      t44 = 0.2D1 * x2 * x3
      t45 = sqrt(x2)
      t52 = Sqrt(x3 * (-0.1D1 + t45) * (t45 + 0.1D1) * t4)
      t54 = 0.2D1 * t23 * t45 * t52
      t59 = x3 * t45
      t61 = t23 * t52
      t66 = t45 * x2
      t67 = t66 * x3
      t75 = x2 * z
      t78 = -t59 * z + 0.2D1 * t61 - 0.2D1 * x2 * t23 * t52 + 0.2D1 * t6
     #7 * z + 0.3D1 * t59 + t45 * z - 0.2D1 * t67 - t66 * z + t66 - 0.2D
     #1 * t45 + 0.2D1 * t75 * t61
      t79 = t78 ** 2
      t81 = (0.1D1 - x2 + t75) ** 2
      t84 = t79 / t81 * t32
      t87 = FJET(XB1, XB2, s, -t2 * (-x3 + t44 - x2 + t54), 0.0D0, t2 * 
     #(0.1D1 - x2 - x3 + t44 + t54), 0.0D0, 0.0D0, -0.3D1 * t31 * t84)
      t92 = -0.1D1 + x1
      t97 = t4 * s
      t103 = 0.1D1 - x1 + x1 * z
      t104 = 0.1D1 / t103
      t108 = Sqrt(-x3 * t103 * t4)
      t109 = t108 ** 2
      t114 = FJET(XB1, XB2, s, -t2 * t92 * x3, t2 * x1 * x3, t97 * t1 * 
     #t92, -t97 * t1 * x1, 0.0D0, -0.24D2 * t31 * t104 * t109 * t24 * t3
     #6)
      bbggh9n1em1 = t41 * t40 - 0.3D1 * t87 * z * wd * t84 - 0.24D2 * t1
     #14 * z * wd * t104 * t109 * t24 * t36

      end function



      doubleprecision function bbggh9n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t2 = s * (-0.1D1 + z)
      t4 = -0.1D1 + x3
      t8 = cos(x4 * 0.3141592653589793D1)
      t9 = t8 ** 2
      t11 = Sqrt(-x3 * t4)
      t12 = t11 ** 2
      t16 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, 0.
     #12D2 * z * wd * t9 * t12)
      bbggh9n1em2 = 0.12D2 * t16 * z * wd * t9 * t12

      end function



      doubleprecision function bbggh9n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      bbggh9n1em3 = 0.0D0

      end function



      doubleprecision function bbggh9n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      bbggh9n1em4 = 0.0D0

      end function


      doubleprecision function bbggh9n2e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t6 = z ** 2
      t8 = 0.1D1 / t6 / z
      t9 = x4 * 0.3141592653589793D1
      t10 = Sin(t9)
      t11 = t10 ** 2
      t12 = t8 * t11
      t16 = log(-0.4D1 * t12 * x3 * t4)
      t19 = lh ** 2
      t20 = 0.180D3 * t19
      t21 = 0.3141592653589793D1 ** 2
      t22 = 0.30D2 * t21
      t23 = t16 ** 2
      t26 = x3 * z
      t28 = Sqrt(-t26 * t4)
      t29 = t28 ** 2
      t44 = cos(t9)
      t45 = t44 ** 2
      t49 = 0.180D3 * lh
      t50 = 0.90D2 + t49
      t51 = t50 * wd
      t52 = t8 * x3
      t54 = sqrt(x2)
      t55 = -0.1D1 + t54
      t56 = t54 + 0.1D1
      t62 = log(0.4D1 * t52 * t4 * t55 * t56 * t11 * x2)
      t63 = x3 * t54
      t64 = t63 * z
      t65 = t54 * z
      t66 = x3 * t55
      t70 = Sqrt(t66 * t56 * z * t4)
      t74 = (t64 - t65 + 0.2D1 * t44 * t70 + t63) ** 2
      t76 = x2 * x3
      t77 = t12 * t4
      t80 = log(-0.4D1 * t76 * t77)
      t86 = t80 ** 2
      t90 = t62 ** 2
      t96 = -t49 - t20 + t22
      t100 = t74 - 0.4D1 * t29 * t45
      t103 = 0.1D1 / x2
      t106 = t4 * t55
      t109 = x1 ** 2
      t114 = log(0.4D1 * t52 * t106 * t56 * t11 * x2 * t109)
      t119 = log(-0.4D1 * t76 * t109 * t77)
      t130 = 0.1D1 / x1
      t133 = t50 * t45
      t134 = x3 * t109
      t137 = log(-0.4D1 * t134 * t77)
      t142 = t45 * wd
      t143 = t137 ** 2
      t147 = t96 * t45
      t154 = -0.2D1 / 0.15D2 * (-(-0.180D3 * t16 * lh - t20 + t22 - 0.45
     #D2 * t23) * t29 + (-t16 * (-t20 + t22) + 0.90D2 * t23 * lh - 0.60D
     #2 * lh * t21 + 0.2884936567583026D3 + 0.120D3 * t19 * lh + 0.15D2 
     #* t23 * t16) * t29) * t45 * wd + (t51 * (-t62 * t74 + 0.4D1 * t80 
     #* t29 * t45) - 0.90D2 * wd * (-0.2D1 * t86 * t29 * t45 + t90 * t74
     # / 0.2D1) + t96 * wd * t100) * t103 / 0.30D2 - (-0.90D2 * wd * (t1
     #14 * t74 - 0.4D1 * t119 * t29 * t45) - t51 * t100) * t103 * t130 /
     # 0.15D2 + (0.4D1 * t133 * wd * t137 * t29 + 0.180D3 * t142 * t143 
     #* t29 - 0.4D1 * t147 * wd * t29) * t130 / 0.15D2
      t155 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t
     #154)
      t157 = x1 * x3
      t159 = -0.1D1 + x1
      t161 = t2 * t159 * x3
      t162 = t4 * s
      t166 = t162 * t1 * t159
      t169 = 0.1D1 / t6
      t170 = t159 ** 2
      t172 = x1 * z
      t173 = -z - x1 + t172
      t174 = 0.1D1 / t173
      t176 = t169 * t170 * t174 * t4
      t179 = log(0.4D1 * t76 * t109 * t11 * t176)
      t184 = Sqrt(x3 * t173 * t4)
      t185 = t184 ** 2
      t186 = t185 * z
      t187 = t186 * t45
      t200 = log(0.4D1 * t134 * t11 * t176)
      t205 = t200 ** 2
      t208 = t174 * t185 * z
      t217 = -(0.360D3 * wd * t179 * t174 * t187 + 0.4D1 * t51 * t174 * 
     #t187) * t103 * t130 / 0.15D2 + (0.4D1 * t133 * wd * t200 * t174 * 
     #t186 + 0.180D3 * t142 * t205 * t208 - 0.4D1 * t147 * wd * t208) * 
     #t130 / 0.15D2
      t218 = FJET(XB1, XB2, s, t2 * t157, -t161, -t162 * t1 * x1, t166, 
     #0.0D0, t217)
      t220 = t157 * z
      t221 = t76 * z
      t222 = t76 * x1
      t223 = t76 * t172
      t228 = Sqrt(-t66 * t56 * t173 * t4)
      t230 = 0.2D1 * t44 * t54 * t228
      t236 = x2 * x1
      t237 = t236 * z
      t238 = z + x1 - t172 - x2 * z - t236 + t237 - t26 - t157 + t220 + 
     #t221 + t222 - t223 + t76 + t230
      t242 = t1 ** 2
      t257 = log(-0.4D1 * t170 * x3 * t106 * t56 * t11 * t169 * t174 * x
     #2 * t109)
      t260 = t54 * t6
      t265 = x3 * t6
      t266 = t54 * x1
      t269 = t44 * t228
      t274 = t54 * x2
      t275 = t109 * t274
      t278 = x1 * t274
      t283 = t54 * t109
      t286 = z * t44 * t228
      t289 = 0.2D1 * t134 * t260 - 0.4D1 * t134 * t65 - 0.3D1 * t265 * t
     #266 - 0.2D1 * t172 * t269 + 0.2D1 * t63 * t172 + 0.2D1 * t275 * t2
     #6 + t278 * t265 - t275 * t265 - 0.2D1 * t236 * t269 + t64 + t275 -
     # 0.2D1 * t283 - t260 + 0.2D1 * t236 * t286
      t313 = 0.2D1 * t134 * t54 + t63 * x1 + 0.3D1 * t266 * t6 + 0.4D1 *
     # t283 * z - 0.2D1 * t283 * t6 + t275 * t6 - t275 * x3 - t278 * x3 
     #+ t278 * z - 0.2D1 * t275 * z + t63 * t6 + 0.2D1 * x1 * t44 * t228
     # - t278 * t6 + 0.2D1 * t286 - 0.3D1 * t266 * z
      t315 = (t289 + t313) ** 2
      t318 = (z + x1 - t172 - t236 + t237) ** 2
      t320 = t174 * t315 / t318
      t325 = -0.90D2 * wd * t257 * z * t320 - t51 * z * t320
      t329 = FJET(XB1, XB2, s, t2 * x1 * (-t26 - t157 + t220 + t221 + t2
     #22 - t223 - x2 + t76 + t230) * t174, -t161, -t2 * x1 * t238 * t174
     #, t166, s * t242 * x2 * x1 * t159 * t174, -t325 * t103 * t130 / 0.
     #15D2)
      bbggh9n2e1 = t155 * t154 + t218 * t217 - t329 * t325 * t103 * t130
     # / 0.15D2

      end function



      doubleprecision function bbggh9n2e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t6 = z ** 2
      t8 = 0.1D1 / t6 / z
      t11 = sqrt(x2)
      t12 = -0.1D1 + t11
      t13 = t11 + 0.1D1
      t15 = x4 * 0.3141592653589793D1
      t16 = Sin(t15)
      t17 = t16 ** 2
      t22 = log(0.4D1 * t8 * x3 * t4 * t12 * t13 * t17 * x2)
      t23 = x3 * t11
      t24 = t23 * z
      t25 = t11 * z
      t26 = cos(t15)
      t27 = x3 * t12
      t31 = Sqrt(t27 * t13 * z * t4)
      t35 = (t24 - t25 + 0.2D1 * t26 * t31 + t23) ** 2
      t37 = x2 * x3
      t38 = t8 * t17
      t39 = t38 * t4
      t42 = log(-0.4D1 * t37 * t39)
      t43 = x3 * z
      t45 = Sqrt(-t43 * t4)
      t46 = t45 ** 2
      t48 = t26 ** 2
      t54 = 0.180D3 * lh
      t55 = 0.90D2 + t54
      t56 = t55 * wd
      t59 = t35 - 0.4D1 * t46 * t48
      t62 = 0.1D1 / x2
      t67 = 0.1D1 / x1
      t68 = t62 * t67
      t71 = t48 * wd
      t72 = x1 ** 2
      t73 = x3 * t72
      t76 = log(-0.4D1 * t73 * t39)
      t90 = log(-0.4D1 * t38 * x3 * t4)
      t96 = lh ** 2
      t98 = 0.3141592653589793D1 ** 2
      t100 = t90 ** 2
      t108 = (-0.90D2 * wd * (-t22 * t35 + 0.4D1 * t42 * t46 * t48) + t5
     #6 * t59) * t62 / 0.30D2 - 0.6D1 * wd * t59 * t68 + (-0.360D3 * t71
     # * t76 * t46 - 0.4D1 * t55 * t48 * wd * t46) * t67 / 0.15D2 - 0.2D
     #1 / 0.15D2 * (-(t54 + 0.90D2 * t90) * t46 + (-0.180D3 * t90 * lh -
     # 0.180D3 * t96 + 0.30D2 * t98 - 0.45D2 * t100) * t46) * t48 * wd
      t109 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t
     #108)
      t111 = x1 * x3
      t113 = -0.1D1 + x1
      t115 = t2 * t113 * x3
      t116 = t4 * s
      t120 = t116 * t1 * t113
      t121 = x1 * z
      t122 = -z - x1 + t121
      t123 = 0.1D1 / t122
      t127 = Sqrt(x3 * t122 * t4)
      t128 = t127 ** 2
      t137 = t113 ** 2
      t142 = log(0.4D1 * t73 * t17 / t6 * t123 * t137 * t4)
      t156 = 0.24D2 * wd * t123 * t128 * z * t48 * t68 + (-0.360D3 * t71
     # * t142 * t123 * t128 * z - 0.4D1 * t56 * t123 * t128 * z * t48) *
     # t67 / 0.15D2
      t157 = FJET(XB1, XB2, s, t2 * t111, -t115, -t116 * t1 * x1, t120, 
     #0.0D0, t156)
      t159 = t111 * z
      t160 = t37 * z
      t161 = t37 * x1
      t162 = t37 * t121
      t167 = Sqrt(-t27 * t13 * t122 * t4)
      t169 = 0.2D1 * t26 * t11 * t167
      t175 = x2 * x1
      t176 = t175 * z
      t177 = z + x1 - t121 - x2 * z - t175 + t176 - t43 - t111 + t159 + 
     #t160 + t161 - t162 + t37 + t169
      t181 = t1 ** 2
      t189 = t11 * x2
      t190 = t72 * t189
      t195 = t26 * t167
      t198 = x3 * t6
      t199 = t11 * x1
      t204 = t11 * t6
      t208 = x1 * t189
      t213 = z * t26 * t167
      t216 = t11 * t72
      t223 = 0.2D1 * t190 * t43 + 0.2D1 * t23 * t121 - 0.2D1 * t121 * t1
     #95 - 0.3D1 * t198 * t199 - 0.4D1 * t73 * t25 + 0.2D1 * t73 * t204 
     #- t190 * t198 + t208 * t198 - 0.2D1 * t175 * t195 + 0.2D1 * t175 *
     # t213 + t24 - 0.2D1 * t216 * t6 + 0.4D1 * t216 * z + 0.3D1 * t199 
     #* t6
      t242 = t23 * x1 + 0.2D1 * t73 * t11 - t208 * x3 - t190 * x3 + t190
     # * t6 - 0.3D1 * t199 * z + 0.2D1 * t213 - t208 * t6 + 0.2D1 * x1 *
     # t26 * t167 + t23 * t6 - 0.2D1 * t190 * z + t208 * z + t190 - 0.2D
     #1 * t216 - t204
      t244 = (t223 + t242) ** 2
      t246 = (z + x1 - t121 - t175 + t176) ** 2
      t249 = t244 / t246 * t68
      t252 = FJET(XB1, XB2, s, t2 * x1 * (-t43 - t111 + t159 + t160 + t1
     #61 - t162 - x2 + t37 + t169) * t123, -t115, -t2 * x1 * t177 * t123
     #, t120, s * t181 * x2 * x1 * t113 * t123, -0.6D1 * wd * z * t123 *
     # t249)
      bbggh9n2e0 = t109 * t108 + t157 * t156 - 0.6D1 * t252 * wd * z * t
     #123 * t249

      end function



      doubleprecision function bbggh9n2em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t8 = Sqrt(-x3 * z * t4)
      t9 = t8 ** 2
      t12 = z ** 2
      t15 = x4 * 0.3141592653589793D1
      t16 = Sin(t15)
      t17 = t16 ** 2
      t22 = log(-0.4D1 / t12 / z * t17 * x3 * t4)
      t27 = cos(t15)
      t28 = t27 ** 2
      t32 = sqrt(x2)
      t33 = x3 * t32
      t42 = Sqrt(x3 * (-0.1D1 + t32) * (t32 + 0.1D1) * z * t4)
      t46 = (t33 * z - t32 * z + 0.2D1 * t27 * t42 + t33) ** 2
      t54 = t28 * wd
      t55 = 0.1D1 / x1
      t59 = -0.2D1 / 0.15D2 * (0.90D2 * t9 + (0.180D3 * lh + 0.90D2 * t2
     #2) * t9) * t28 * wd - 0.3D1 * wd * (t46 - 0.4D1 * t9 * t28) / x2 +
     # 0.24D2 * t54 * t9 * t55
      t60 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t5
     #9)
      t64 = -0.1D1 + x1
      t67 = t4 * s
      t73 = -z - x1 + x1 * z
      t74 = 0.1D1 / t73
      t78 = Sqrt(x3 * t73 * t4)
      t79 = t78 ** 2
      t84 = FJET(XB1, XB2, s, t2 * x1 * x3, -t2 * t64 * x3, -t67 * t1 * 
     #x1, t67 * t1 * t64, 0.0D0, 0.24D2 * t54 * t74 * t79 * z * t55)
      bbggh9n2em1 = t60 * t59 + 0.24D2 * t84 * t28 * wd * t74 * t79 * z 
     #* t55

      end function



      doubleprecision function bbggh9n2em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t2 = s * (-0.1D1 + z)
      t4 = -0.1D1 + x3
      t7 = cos(x4 * 0.3141592653589793D1)
      t8 = t7 ** 2
      t12 = Sqrt(-x3 * z * t4)
      t13 = t12 ** 2
      t16 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.
     #12D2 * t8 * wd * t13)
      bbggh9n2em2 = 0.12D2 * t16 * t8 * wd * t13

      end function



      doubleprecision function bbggh9n2em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      bbggh9n2em3 = 0.0D0

      end function



      doubleprecision function bbggh9n2em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      bbggh9n2em4 = 0.0D0

      end function
