  
      subroutine bbarbbarH8n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision bbarbbarH8n1e1  
      doubleprecision bbarbbarH8n1e0  
      doubleprecision bbarbbarH8n1em1  
      doubleprecision bbarbbarH8n1em2  
      doubleprecision bbarbbarH8n1em3  
      doubleprecision bbarbbarH8n1em4  
      doubleprecision bbarbbarH8n2e1  
      doubleprecision bbarbbarH8n2e0  
      doubleprecision bbarbbarH8n2em1  
      doubleprecision bbarbbarH8n2em2  
      doubleprecision bbarbbarH8n2em3  
      doubleprecision bbarbbarH8n2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=bbarbbarH8n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarbbarH8n2e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=bbarbbarH8n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarbbarH8n2e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=bbarbbarH8n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarbbarH8n2em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=bbarbbarH8n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarbbarH8n2em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=bbarbbarH8n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarbbarH8n2em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=bbarbbarH8n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarbbarH8n2em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function bbarbbarH8n1e1
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
      t17 = lh ** 2
      t19 = 0.3141592653589793D1 ** 2
      t21 = 0.180D3 * t17 - 0.30D2 * t19
      t24 = t15 ** 2
      t29 = wd * z
      t41 = cos(t6)
      t42 = t41 ** 2
      t44 = x3 * t4
      t45 = Sqrt(-t44)
      t46 = t45 ** 2
      t49 = t29 * lh
      t50 = x2 * x3
      t52 = t8 * t11 * t4
      t55 = log(-0.4D1 * t50 * t52)
      t60 = t55 ** 2
      t67 = t29 * t21 * t42 * t46
      t70 = 0.1D1 / x2
      t73 = x1 ** 2
      t77 = log(-0.4D1 * t50 * t73 * t52)
      t88 = 0.1D1 / x1
      t91 = x3 * t73
      t94 = log(-0.4D1 * t91 * t52)
      t99 = t94 ** 2
      t107 = -(-t15 * wd * z * t21 - 0.90D2 * t24 * wd * z * lh + t29 * 
     #(-0.2884936567583026D3 - 0.120D3 * t17 * lh + 0.60D2 * lh * t19) -
     # 0.15D2 * t24 * t15 * wd * z) * t42 * t46 / 0.45D2 + (-0.720D3 * t
     #49 * t55 * t42 * t46 - 0.180D3 * t29 * t60 * t42 * t46 - 0.4D1 * t
     #67) * t70 / 0.180D3 + (0.360D3 * t29 * t77 * t42 * t46 + 0.720D3 *
     # t29 * lh * t42 * t46) * t70 * t88 / 0.90D2 - 0.2D1 / 0.45D2 * (0.
     #180D3 * t49 * t42 * t94 * t46 + 0.45D2 * t29 * t42 * t99 * t46 + t
     #67) * t88
      t108 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t
     #107)
      t110 = -0.1D1 + x1
      t111 = x1 * x3
      t112 = t111 * z
      t113 = 0.2D1 * t50
      t114 = t50 * x1
      t115 = x1 * z
      t116 = t50 * t115
      t117 = sqrt(x2)
      t118 = t41 * t117
      t119 = -0.1D1 + t117
      t120 = x3 * t119
      t121 = t117 + 0.1D1
      t122 = 0.1D1 - x1 + t115
      t126 = Sqrt(t120 * t121 * t122 * t4)
      t128 = 0.2D1 * t118 * t126
      t131 = 0.1D1 / t122
      t134 = t2 * t111
      t135 = x2 * x1
      t136 = t135 * z
      t137 = 0.1D1 - x1 + t115 - x2 + t135 - t136 - x3 + t111 - t112 + t
     #113 - t114 + t116 + t128
      t141 = t4 * s
      t143 = t141 * t1 * x1
      t144 = t1 ** 2
      t154 = t110 ** 2
      t160 = log(0.4D1 * t131 * x2 * t11 * t119 * t121 * t44 * t8 * t154
     # * t73)
      t162 = x2 * z
      t164 = (-0.1D1 + x1 - t115 + x2 - t135 - t162 + t136) ** 2
      t167 = t117 * x2
      t168 = t167 * x3
      t171 = t41 * x2
      t177 = t126 * z
      t180 = t117 * z
      t184 = t117 * x1
      t192 = x3 * t117
      t198 = 0.2D1 * t168 * z
      t199 = t167 * z
      t202 = t41 * t126
      t209 = t73 * t167
      t216 = 0.4D1 * t168 * t115 - 0.2D1 * t171 * t126 * x1 - t168 * t10
     # * x1 - 0.2D1 * t171 * t177 + 0.4D1 * t91 * t180 + x3 * t10 * t184
     # - 0.2D1 * t168 * t73 * z - 0.2D1 * t91 * t10 * t117 - 0.6D1 * t19
     #2 * t115 + t168 * t10 * t73 - t198 - 0.3D1 * t199 * x1 + 0.2D1 * t
     #202 * x1 + 0.2D1 * t171 * t126 + t167 * t10 * x1 + 0.2D1 * t209 * 
     #z - t209 * t10 - 0.3D1 * t168 * x1 + t168 * t73
      t217 = t192 * z
      t222 = 0.2D1 * t117
      t223 = 0.2D1 * t168
      t224 = 0.3D1 * t192
      t232 = t117 * t73
      t243 = t217 + 0.5D1 * t184 * z - 0.2D1 * t91 * t117 + t222 + t199 
     #+ t223 - t224 - t180 - 0.4D1 * t184 - 0.2D1 * t202 - t167 - 0.2D1 
     #* t202 * t115 + 0.2D1 * t171 * t177 * x1 + 0.2D1 * t232 - t209 + 0
     #.2D1 * t167 * x1 + 0.2D1 * t232 * t10 - 0.4D1 * t232 * z - t184 * 
     #t10 + 0.5D1 * t192 * x1
      t245 = (t216 + t243) ** 2
      t246 = 0.1D1 / t164 * t131 * t245
      t251 = 0.90D2 * t29 * t160 * t246 + 0.180D3 * t49 * t246
      t255 = FJET(XB1, XB2, s, t2 * t110 * (-x3 + t111 - t112 + t113 - t
     #114 + t116 - x2 + t128) * t131, t134, -t2 * t110 * t137 * t131, -t
     #143, -s * t144 * x2 * t110 * x1 * t131, t251 * t70 * t88 / 0.90D2)
      t262 = Sqrt(t120 * t121 * t4)
      t264 = 0.2D1 * t118 * t262
      t273 = t4 * t8 * t119 * t121
      t276 = log(0.4D1 * x2 * t11 * x3 * t273)
      t278 = (0.1D1 - x2 + t162) ** 2
      t279 = 0.1D1 / t278
      t288 = -t199 - t223 + 0.2D1 * t41 * t262 + t224 + t180 - 0.2D1 * t
     #171 * t262 + t198 + 0.2D1 * t171 * t262 * z - t222 + t167 - t217
      t289 = t288 ** 2
      t293 = t276 ** 2
      t309 = log(0.4D1 * x2 * t73 * t11 * x3 * t273)
      t322 = (0.180D3 * t49 * t276 * t279 * t289 + 0.45D2 * t29 * t293 *
     # t279 * t289 + t29 * t21 * t279 * t289) * t70 / 0.180D3 + (-0.90D2
     # * t29 * t309 * t279 * t289 - 0.180D3 * t29 * lh * t279 * t289) * 
     #t70 * t88 / 0.90D2
      t323 = FJET(XB1, XB2, s, -t2 * (-x3 + t113 - x2 + t264), 0.0D0, t2
     # * (0.1D1 - x2 - x3 + t113 + t264), 0.0D0, 0.0D0, t322)
      t333 = t11 * t131 * t154 * t4
      t336 = log(-0.4D1 * t50 * t73 * t8 * t333)
      t341 = Sqrt(-x3 * t122 * t4)
      t342 = t341 ** 2
      t343 = t131 * t42 * t342
      t355 = log(-0.4D1 * t91 * t8 * t333)
      t362 = t355 ** 2
      t372 = (-0.360D3 * t29 * t336 * t343 - 0.720D3 * t49 * t343) * t70
     # * t88 / 0.90D2 - 0.2D1 / 0.45D2 * (-0.180D3 * t49 * t42 * t355 * 
     #t131 * t342 - 0.45D2 * t29 * t42 * t362 * t131 * t342 - t29 * t21 
     #* t343) * t88
      t373 = FJET(XB1, XB2, s, -t2 * t110 * x3, t134, t141 * t1 * t110, 
     #-t143, 0.0D0, t372)
      bbarbbarH8n1e1 = t108 * t107 + t255 * t251 * t70 * t88 / 0.90D2 + 
     #t323 * t322 + t373 * t372

      end function



      doubleprecision function bbarbbarH8n1e0
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
      t6 = wd * z
      t7 = x2 * x3
      t8 = x4 * 0.3141592653589793D1
      t9 = Sin(t8)
      t10 = t9 ** 2
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t14 = t10 * t12 * t4
      t17 = log(-0.4D1 * t7 * t14)
      t18 = cos(t8)
      t19 = t18 ** 2
      t22 = Sqrt(-x3 * t4)
      t23 = t22 ** 2
      t29 = t6 * lh * t19 * t23
      t32 = 0.1D1 / x2
      t35 = t6 * t19
      t37 = 0.1D1 / x1
      t41 = x1 ** 2
      t42 = x3 * t41
      t45 = log(-0.4D1 * t42 * t14)
      t58 = log(-0.4D1 * x3 * t10 * t12 * t4)
      t63 = lh ** 2
      t65 = 0.3141592653589793D1 ** 2
      t69 = t58 ** 2
      t77 = (0.360D3 * t6 * t17 * t19 * t23 + 0.720D3 * t29) * t32 / 0.1
     #80D3 - 0.4D1 * t35 * t23 * t32 * t37 - 0.2D1 / 0.45D2 * (-0.90D2 *
     # t6 * t19 * t45 * t23 - 0.180D3 * t29) * t37 - (0.180D3 * t58 * wd
     # * z * lh + t6 * (0.180D3 * t63 - 0.30D2 * t65) + 0.45D2 * t69 * w
     #d * z) * t19 * t23 / 0.45D2
      t78 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t7
     #7)
      t80 = -0.1D1 + x1
      t81 = x1 * x3
      t82 = t81 * z
      t83 = 0.2D1 * t7
      t84 = t7 * x1
      t85 = x1 * z
      t86 = t7 * t85
      t87 = sqrt(x2)
      t88 = t18 * t87
      t89 = -0.1D1 + t87
      t90 = x3 * t89
      t91 = t87 + 0.1D1
      t92 = 0.1D1 - x1 + t85
      t96 = Sqrt(t90 * t91 * t92 * t4)
      t98 = 0.2D1 * t88 * t96
      t101 = 0.1D1 / t92
      t104 = t2 * t81
      t105 = x2 * x1
      t106 = t105 * z
      t107 = 0.1D1 - x1 + t85 - x2 + t105 - t106 - x3 + t81 - t82 + t83 
     #- t84 + t86 + t98
      t111 = t4 * s
      t113 = t111 * t1 * x1
      t114 = t1 ** 2
      t120 = x2 * z
      t122 = (-0.1D1 + x1 - t85 + x2 - t105 - t120 + t106) ** 2
      t123 = 0.1D1 / t122
      t125 = t87 * x1
      t127 = t87 * t41
      t132 = t18 * x2
      t135 = t18 * t96
      t138 = t87 * x2
      t139 = t138 * z
      t144 = t41 * t138
      t148 = t138 * x3
      t156 = x3 * t87
      t157 = t156 * z
      t161 = 0.2D1 * t148 * z
      t169 = -t125 * t11 - 0.4D1 * t127 * z + 0.2D1 * t127 * t11 + 0.2D1
     # * t132 * t96 + 0.2D1 * t135 * x1 - 0.3D1 * t139 * x1 + t138 * t11
     # * x1 + 0.2D1 * t144 * z - t144 * t11 + t148 * t41 - 0.3D1 * t148 
     #* x1 - 0.2D1 * t42 * t87 + 0.5D1 * t125 * z + t157 + 0.5D1 * t156 
     #* x1 - t161 + 0.4D1 * t148 * t85 - 0.2D1 * t132 * t96 * x1 - 0.2D1
     # * t135 * t85
      t170 = t96 * z
      t187 = t87 * z
      t193 = 0.2D1 * t87
      t194 = 0.2D1 * t148
      t195 = 0.3D1 * t156
      t201 = -0.2D1 * t132 * t170 + t148 * t11 * t41 - t148 * t11 * x1 +
     # x3 * t11 * t125 - 0.2D1 * t148 * t41 * z - 0.6D1 * t156 * t85 - 0
     #.2D1 * t42 * t11 * t87 + 0.4D1 * t42 * t187 + 0.2D1 * t132 * t170 
     #* x1 - t138 + t193 + t139 + t194 - t195 - t187 - 0.4D1 * t125 - 0.
     #2D1 * t135 + 0.2D1 * t138 * x1 - t144 + 0.2D1 * t127
      t203 = (t169 + t201) ** 2
      t205 = t32 * t37
      t206 = t101 * t203 * t205
      t208 = FJET(XB1, XB2, s, t2 * t80 * (-x3 + t81 - t82 + t83 - t84 +
     # t86 - x2 + t98) * t101, t104, -t2 * t80 * t107 * t101, -t113, -s 
     #* t114 * x2 * t80 * x1 * t101, -t6 * t123 * t206)
      t215 = Sqrt(t90 * t91 * t4)
      t217 = 0.2D1 * t88 * t215
      t229 = log(0.4D1 * x2 * t12 * x3 * t4 * t10 * t89 * t91)
      t231 = (0.1D1 - x2 + t120) ** 2
      t232 = 0.1D1 / t231
      t241 = -t139 - t194 + 0.2D1 * t18 * t215 + t195 + t187 - 0.2D1 * t
     #132 * t215 + t161 + 0.2D1 * t132 * t215 * z - t193 + t138 - t157
      t242 = t241 ** 2
      t257 = (-0.90D2 * t6 * t229 * t232 * t242 - 0.180D3 * t6 * lh * t2
     #32 * t242) * t32 / 0.180D3 + t6 * t232 * t242 * t32 * t37
      t258 = FJET(XB1, XB2, s, -t2 * (-x3 + t83 - x2 + t217), 0.0D0, t2 
     #* (0.1D1 - x2 - x3 + t83 + t217), 0.0D0, 0.0D0, t257)
      t267 = Sqrt(-x3 * t92 * t4)
      t268 = t267 ** 2
      t275 = t80 ** 2
      t280 = log(-0.4D1 * t42 * t10 * t12 * t101 * t275 * t4)
      t293 = 0.4D1 * t6 * t101 * t19 * t268 * t205 - 0.2D1 / 0.45D2 * (0
     #.90D2 * t35 * t280 * t101 * t268 + 0.180D3 * t6 * lh * t101 * t19 
     #* t268) * t37
      t294 = FJET(XB1, XB2, s, -t2 * t80 * x3, t104, t111 * t1 * t80, -t
     #113, 0.0D0, t293)
      bbarbbarH8n1e0 = t78 * t77 - t208 * wd * z * t123 * t206 + t258 * 
     #t257 + t294 * t293

      end function



      doubleprecision function bbarbbarH8n1em1
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
      t6 = wd * z
      t9 = x4 * 0.3141592653589793D1
      t10 = Sin(t9)
      t11 = t10 ** 2
      t13 = z ** 2
      t18 = log(-0.4D1 * x3 * t11 / t13 * t4)
      t23 = cos(t9)
      t24 = t23 ** 2
      t27 = Sqrt(-x3 * t4)
      t28 = t27 ** 2
      t31 = t24 * t28
      t32 = 0.1D1 / x2
      t36 = 0.1D1 / x1
      t40 = -(-0.180D3 * t6 * lh - 0.90D2 * t18 * wd * z) * t24 * t28 / 
     #0.45D2 - 0.2D1 * t6 * t31 * t32 - 0.4D1 * t6 * t31 * t36
      t41 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t4
     #0)
      t44 = 0.2D1 * x2 * x3
      t45 = sqrt(x2)
      t52 = Sqrt(x3 * (-0.1D1 + t45) * (t45 + 0.1D1) * t4)
      t54 = 0.2D1 * t23 * t45 * t52
      t61 = (0.1D1 - x2 + x2 * z) ** 2
      t63 = t45 * x2
      t65 = t63 * x3
      t69 = x3 * t45
      t72 = t23 * x2
      t82 = -t63 * z - 0.2D1 * t65 + 0.2D1 * t23 * t52 + 0.3D1 * t69 + t
     #45 * z - 0.2D1 * t72 * t52 + 0.2D1 * t65 * z + 0.2D1 * t72 * t52 *
     # z - 0.2D1 * t45 + t63 - t69 * z
      t83 = t82 ** 2
      t85 = 0.1D1 / t61 * t83 * t32
      t88 = FJET(XB1, XB2, s, -t2 * (-x3 + t44 - x2 + t54), 0.0D0, t2 * 
     #(0.1D1 - x2 - x3 + t44 + t54), 0.0D0, 0.0D0, t6 * t85 / 0.2D1)
      t93 = -0.1D1 + x1
      t98 = t4 * s
      t104 = 0.1D1 - x1 + x1 * z
      t105 = 0.1D1 / t104
      t109 = Sqrt(-x3 * t104 * t4)
      t110 = t109 ** 2
      t115 = FJET(XB1, XB2, s, -t2 * t93 * x3, t2 * x1 * x3, t98 * t1 * 
     #t93, -t98 * t1 * x1, 0.0D0, 0.4D1 * t6 * t105 * t24 * t110 * t36)
      bbarbbarH8n1em1 = t41 * t40 + t88 * wd * z * t85 / 0.2D1 + 0.4D1 *
     # t115 * wd * z * t105 * t24 * t110 * t36

      end function



      doubleprecision function bbarbbarH8n1em2
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
      t16 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, -0
     #.2D1 * wd * z * t9 * t12)
      bbarbbarH8n1em2 = -0.2D1 * t16 * wd * z * t9 * t12

      end function



      doubleprecision function bbarbbarH8n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      bbarbbarH8n1em3 = 0.0D0

      end function



      doubleprecision function bbarbbarH8n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      bbarbbarH8n1em4 = 0.0D0

      end function


      doubleprecision function bbarbbarH8n2e1
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
      t17 = cos(t9)
      t18 = t17 ** 2
      t20 = lh ** 2
      t22 = 0.3141592653589793D1 ** 2
      t24 = 0.180D3 * t20 - 0.30D2 * t22
      t25 = wd * t24
      t27 = t16 ** 2
      t29 = wd * lh
      t32 = t18 * wd
      t44 = x3 * z
      t46 = Sqrt(-t44 * t4)
      t47 = t46 ** 2
      t53 = sqrt(x2)
      t54 = -0.1D1 + t53
      t55 = t53 + 0.1D1
      t57 = t4 * t11 * t54 * t55
      t60 = log(0.4D1 * x2 * t8 * x3 * t57)
      t61 = t53 * z
      t62 = x3 * t53
      t63 = t62 * z
      t64 = x3 * t54
      t68 = Sqrt(t64 * t55 * z * t4)
      t72 = (-t61 + t63 + 0.2D1 * t17 * t68 + t62) ** 2
      t74 = x2 * x3
      t75 = t12 * t4
      t78 = log(-0.4D1 * t74 * t75)
      t85 = t78 ** 2
      t89 = t60 ** 2
      t97 = t72 - 0.4D1 * t18 * t47
      t100 = 0.1D1 / x2
      t103 = x1 ** 2
      t104 = x2 * t103
      t109 = log(0.4D1 * t104 * t8 * x3 * t57)
      t114 = log(-0.4D1 * t74 * t103 * t75)
      t125 = 0.1D1 / x1
      t128 = x3 * t103
      t131 = log(-0.4D1 * t128 * t75)
      t136 = t131 ** 2
      t146 = -(-t16 * t18 * t25 - 0.90D2 * t27 * t18 * t29 + t32 * (-0.2
     #884936567583026D3 - 0.120D3 * t20 * lh + 0.60D2 * lh * t22) - 0.15
     #D2 * t27 * t16 * t18 * wd) * t47 / 0.45D2 + (-0.180D3 * t29 * (-t6
     #0 * t72 + 0.4D1 * t78 * t18 * t47) + 0.90D2 * wd * (-0.2D1 * t85 *
     # t18 * t47 + t89 * t72 / 0.2D1) + t25 * t97) * t100 / 0.180D3 + (0
     #.90D2 * wd * (-t109 * t72 + 0.4D1 * t114 * t18 * t47) - 0.180D3 * 
     #t29 * t97) * t100 * t125 / 0.90D2 - (0.720D3 * t32 * lh * t131 * t
     #47 + 0.180D3 * t32 * t136 * t47 + 0.4D1 * t32 * t24 * t47) * t125 
     #/ 0.90D2
      t147 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t
     #146)
      t149 = x1 * x3
      t151 = -0.1D1 + x1
      t153 = t2 * t151 * x3
      t154 = t4 * s
      t158 = t154 * t1 * t151
      t161 = 0.1D1 / t6
      t162 = t151 ** 2
      t164 = x1 * z
      t165 = -z - x1 + t164
      t166 = 0.1D1 / t165
      t168 = t161 * t162 * t166 * t4
      t171 = log(0.4D1 * t74 * t103 * t11 * t168)
      t176 = Sqrt(x3 * t165 * t4)
      t177 = t176 ** 2
      t179 = t177 * t166 * z
      t182 = t29 * t18
      t191 = log(0.4D1 * t128 * t11 * t168)
      t197 = t191 ** 2
      t207 = (0.360D3 * wd * t171 * t18 * t179 + 0.720D3 * t182 * t179) 
     #* t100 * t125 / 0.90D2 - (0.720D3 * t182 * t191 * t177 * t166 * z 
     #+ 0.180D3 * t32 * t197 * t179 + 0.4D1 * t32 * t24 * t179) * t125 /
     # 0.90D2
      t208 = FJET(XB1, XB2, s, t2 * t149, -t153, -t154 * t1 * x1, t158, 
     #0.0D0, t207)
      t210 = t149 * z
      t211 = t74 * z
      t212 = t74 * x1
      t213 = t74 * t164
      t218 = Sqrt(-t64 * t55 * t165 * t4)
      t220 = 0.2D1 * t17 * t53 * t218
      t226 = x2 * x1
      t227 = t226 * z
      t228 = z + x1 - t164 - x2 * z - t226 + t227 - t44 - t149 + t210 + 
     #t211 + t212 - t213 + t74 + t220
      t232 = t1 ** 2
      t247 = log(-0.4D1 * t104 * t161 * x3 * t4 * t11 * t166 * t162 * t5
     #4 * t55)
      t251 = (z + x1 - t164 - t226 + t227) ** 2
      t256 = t53 * t6
      t261 = t17 * t218
      t264 = t17 * x2
      t265 = t218 * x1
      t268 = t53 * x2
      t269 = x3 * t268
      t278 = t53 * x1
      t284 = t103 * t268
      t285 = t53 * t103
      t289 = -0.4D1 * t128 * t61 + 0.2D1 * t128 * t256 + 0.2D1 * t62 * t
     #164 - 0.2D1 * t261 * t164 - 0.2D1 * t264 * t265 - t269 * t6 * t103
     # + t269 * t6 * x1 + 0.2D1 * t269 * t103 * z - 0.3D1 * x3 * t6 * t2
     #78 + 0.2D1 * t264 * t265 * z - t256 + t284 - 0.2D1 * t285 + t268 *
     # z * x1
      t313 = -t268 * t6 * x1 - 0.2D1 * t284 * z + t284 * t6 + t62 * t6 -
     # t269 * t103 - t269 * x1 + 0.2D1 * t261 * z + 0.2D1 * t261 * x1 + 
     #0.2D1 * t128 * t53 - 0.3D1 * t278 * z + t62 * x1 + 0.3D1 * t278 * 
     #t6 + 0.4D1 * t285 * z - 0.2D1 * t285 * t6 + t63
      t315 = (t289 + t313) ** 2
      t316 = t166 / t251 * t315
      t322 = -0.90D2 * wd * t247 * z * t316 - 0.180D3 * t29 * z * t316
      t326 = FJET(XB1, XB2, s, t2 * x1 * (-t44 - t149 + t210 + t211 + t2
     #12 - t213 - x2 + t74 + t220) * t166, -t153, -t2 * x1 * t228 * t166
     #, t158, s * t232 * x2 * x1 * t151 * t166, t322 * t100 * t125 / 0.9
     #0D2)
      bbarbbarH8n2e1 = t147 * t146 + t208 * t207 + t326 * t322 * t100 * 
     #t125 / 0.90D2

      end function



      doubleprecision function bbarbbarH8n2e0
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
      t11 = x4 * 0.3141592653589793D1
      t12 = Sin(t11)
      t13 = t12 ** 2
      t15 = sqrt(x2)
      t16 = -0.1D1 + t15
      t17 = t15 + 0.1D1
      t22 = log(0.4D1 * x2 * t8 * x3 * t4 * t13 * t16 * t17)
      t23 = t15 * z
      t24 = x3 * t15
      t25 = t24 * z
      t26 = cos(t11)
      t27 = x3 * t16
      t31 = Sqrt(t27 * t17 * z * t4)
      t35 = (-t23 + t25 + 0.2D1 * t26 * t31 + t24) ** 2
      t37 = x2 * x3
      t38 = t8 * t13
      t39 = t38 * t4
      t42 = log(-0.4D1 * t37 * t39)
      t43 = t26 ** 2
      t45 = x3 * z
      t47 = Sqrt(-t45 * t4)
      t48 = t47 ** 2
      t54 = wd * lh
      t57 = t35 - 0.4D1 * t43 * t48
      t61 = 0.1D1 / x2
      t65 = 0.1D1 / x1
      t66 = t61 * t65
      t68 = t43 * wd
      t69 = x1 ** 2
      t70 = x3 * t69
      t73 = log(-0.4D1 * t70 * t39)
      t86 = log(-0.4D1 * t38 * x3 * t4)
      t90 = lh ** 2
      t92 = 0.3141592653589793D1 ** 2
      t96 = t86 ** 2
      t103 = (0.90D2 * wd * (-t22 * t35 + 0.4D1 * t42 * t43 * t48) - 0.1
     #80D3 * t54 * t57) * t61 / 0.180D3 + wd * t57 * t66 - (-0.360D3 * t
     #68 * t73 * t48 - 0.720D3 * t68 * lh * t48) * t65 / 0.90D2 - (0.180
     #D3 * t86 * t43 * t54 + t68 * (0.180D3 * t90 - 0.30D2 * t92) + 0.45
     #D2 * t96 * t43 * wd) * t48 / 0.45D2
      t104 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t
     #103)
      t106 = x1 * x3
      t108 = -0.1D1 + x1
      t110 = t2 * t108 * x3
      t111 = t4 * s
      t115 = t111 * t1 * t108
      t116 = x1 * z
      t117 = -z - x1 + t116
      t120 = Sqrt(x3 * t117 * t4)
      t121 = t120 ** 2
      t123 = 0.1D1 / t117
      t124 = t123 * z
      t131 = t108 ** 2
      t136 = log(0.4D1 * t70 * t13 / t6 * t123 * t131 * t4)
      t139 = t121 * t123 * z
      t148 = -0.4D1 * t68 * t121 * t124 * t66 - (-0.360D3 * t68 * t136 *
     # t139 - 0.720D3 * t54 * t43 * t139) * t65 / 0.90D2
      t149 = FJET(XB1, XB2, s, t2 * t106, -t110, -t111 * t1 * x1, t115, 
     #0.0D0, t148)
      t151 = t106 * z
      t152 = t37 * z
      t153 = t37 * x1
      t154 = t37 * t116
      t159 = Sqrt(-t27 * t17 * t117 * t4)
      t161 = 0.2D1 * t26 * t15 * t159
      t167 = x2 * x1
      t168 = t167 * z
      t169 = z + x1 - t116 - x2 * z - t167 + t168 - t45 - t106 + t151 + 
     #t152 + t153 - t154 + t37 + t161
      t173 = t1 ** 2
      t182 = (z + x1 - t116 - t167 + t168) ** 2
      t184 = t26 * t159
      t187 = t26 * x2
      t188 = t159 * x1
      t191 = t15 * x2
      t192 = x3 * t191
      t201 = t15 * x1
      t206 = t15 * t6
      t215 = t69 * t191
      t220 = -0.2D1 * t184 * t116 - 0.2D1 * t187 * t188 - t192 * t6 * t6
     #9 + t192 * t6 * x1 + 0.2D1 * t192 * t69 * z - 0.3D1 * x3 * t6 * t2
     #01 - 0.4D1 * t70 * t23 + 0.2D1 * t70 * t206 + 0.2D1 * t24 * t116 +
     # t191 * z * x1 - t191 * t6 * x1 - 0.2D1 * t215 * z + t215 * t6 + t
     #24 * t6
      t234 = t15 * t69
      t243 = -t192 * t69 - t192 * x1 + 0.2D1 * t184 * z + 0.2D1 * t184 *
     # x1 + 0.2D1 * t70 * t15 - 0.3D1 * t201 * z + t24 * x1 + 0.3D1 * t2
     #01 * t6 + 0.4D1 * t234 * z - 0.2D1 * t234 * t6 + t25 + 0.2D1 * t18
     #7 * t188 * z - t206 + t215 - 0.2D1 * t234
      t245 = (t220 + t243) ** 2
      t247 = 0.1D1 / t182 * t245 * t66
      t249 = FJET(XB1, XB2, s, t2 * x1 * (-t45 - t106 + t151 + t152 + t1
     #53 - t154 - x2 + t37 + t161) * t123, -t110, -t2 * x1 * t169 * t123
     #, t115, s * t173 * x2 * x1 * t108 * t123, wd * z * t123 * t247)
      bbarbbarH8n2e0 = t104 * t103 + t149 * t148 + t249 * wd * t124 * t2
     #47

      end function



      doubleprecision function bbarbbarH8n2em1
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
      t7 = cos(t6)
      t8 = t7 ** 2
      t9 = t8 * wd
      t12 = z ** 2
      t15 = Sin(t6)
      t16 = t15 ** 2
      t21 = log(-0.4D1 / t12 / z * t16 * x3 * t4)
      t28 = Sqrt(-x3 * z * t4)
      t29 = t28 ** 2
      t32 = sqrt(x2)
      t34 = x3 * t32
      t42 = Sqrt(x3 * (-0.1D1 + t32) * (t32 + 0.1D1) * z * t4)
      t46 = (-t32 * z + t34 * z + 0.2D1 * t7 * t42 + t34) ** 2
      t54 = 0.1D1 / x1
      t58 = -(-0.180D3 * t9 * lh - 0.90D2 * t21 * t8 * wd) * t29 / 0.45D
     #2 + wd * (t46 - 0.4D1 * t8 * t29) / x2 / 0.2D1 - 0.4D1 * t9 * t29 
     #* t54
      t59 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t5
     #8)
      t63 = -0.1D1 + x1
      t66 = t4 * s
      t72 = -z - x1 + x1 * z
      t75 = Sqrt(x3 * t72 * t4)
      t76 = t75 ** 2
      t78 = 0.1D1 / t72
      t83 = FJET(XB1, XB2, s, t2 * x1 * x3, -t2 * t63 * x3, -t66 * t1 * 
     #x1, t66 * t1 * t63, 0.0D0, -0.4D1 * t9 * t76 * t78 * z * t54)
      bbarbbarH8n2em1 = t59 * t58 - 0.4D1 * t83 * t8 * wd * t76 * t78 * 
     #z * t54

      end function



      doubleprecision function bbarbbarH8n2em2
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
      t16 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, -0
     #.2D1 * t8 * wd * t13)
      bbarbbarH8n2em2 = -0.2D1 * t16 * t8 * wd * t13

      end function



      doubleprecision function bbarbbarH8n2em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      bbarbbarH8n2em3 = 0.0D0

      end function



      doubleprecision function bbarbbarH8n2em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      bbarbbarH8n2em4 = 0.0D0

      end function
