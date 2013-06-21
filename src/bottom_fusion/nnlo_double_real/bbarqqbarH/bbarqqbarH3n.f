  
      subroutine bbarqqbarH3n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision bbarqqbarH3n1e1  
      doubleprecision bbarqqbarH3n1e0  
      doubleprecision bbarqqbarH3n1em1  
      doubleprecision bbarqqbarH3n1em2  
      doubleprecision bbarqqbarH3n1em3  
      doubleprecision bbarqqbarH3n1em4  
      doubleprecision bbarqqbarH3n2e1  
      doubleprecision bbarqqbarH3n2e0  
      doubleprecision bbarqqbarH3n2em1  
      doubleprecision bbarqqbarH3n2em2  
      doubleprecision bbarqqbarH3n2em3  
      doubleprecision bbarqqbarH3n2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=bbarqqbarH3n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarqqbarH3n2e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=bbarqqbarH3n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarqqbarH3n2e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=bbarqqbarH3n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarqqbarH3n2em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=bbarqqbarH3n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarqqbarH3n2em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=bbarqqbarH3n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarqqbarH3n2em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=bbarqqbarH3n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarqqbarH3n2em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function bbarqqbarH3n1e1
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
      t9 = z ** 2
      t10 = 0.1D1 / t9
      t11 = t8 * t10
      t12 = x3 * t4
      t15 = log(-0.4D1 * t11 * t12)
      t17 = lh ** 2
      t19 = 0.3141592653589793D1 ** 2
      t21 = 0.180D3 * t17 - 0.30D2 * t19
      t24 = t15 ** 2
      t29 = wd * z
      t41 = cos(t6)
      t42 = t41 ** 2
      t44 = Sqrt(-t12)
      t45 = t44 ** 2
      t48 = t29 * lh
      t51 = t10 * x3 * t4
      t54 = log(-0.4D1 * x2 * t8 * t51)
      t59 = t54 ** 2
      t66 = t29 * t21 * t42 * t45
      t69 = 0.1D1 / x2
      t72 = x1 ** 2
      t73 = x2 * t72
      t77 = log(-0.4D1 * t73 * t8 * t51)
      t88 = 0.1D1 / x1
      t91 = t72 * t8
      t92 = t91 * t51
      t94 = log(-0.4D1 * t92)
      t99 = t94 ** 2
      t107 = -(-t15 * wd * z * t21 - 0.90D2 * t24 * wd * z * lh + t29 * 
     #(0.60D2 * lh * t19 - 0.2884936567583026D3 - 0.120D3 * t17 * lh) - 
     #0.15D2 * t24 * t15 * wd * z) * t42 * t45 / 0.45D2 - (0.720D3 * t48
     # * t54 * t42 * t45 + 0.180D3 * t29 * t59 * t42 * t45 + 0.4D1 * t66
     #) * t69 / 0.180D3 - (-0.360D3 * t29 * t77 * t42 * t45 - 0.720D3 * 
     #t29 * lh * t42 * t45) * t69 * t88 / 0.90D2 - 0.2D1 / 0.45D2 * (0.1
     #80D3 * t48 * t42 * t94 * t45 + 0.45D2 * t29 * t42 * t99 * t45 + t6
     #6) * t88
      t108 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t
     #107)
      t110 = -0.1D1 + x1
      t111 = x3 * x1
      t112 = t111 * z
      t113 = x2 * x3
      t114 = 0.2D1 * t113
      t115 = t113 * x1
      t116 = x1 * z
      t117 = t113 * t116
      t118 = sqrt(x2)
      t119 = t41 * t118
      t120 = -0.1D1 + t118
      t121 = x3 * t120
      t122 = t118 + 0.1D1
      t123 = 0.1D1 - x1 + t116
      t127 = Sqrt(t121 * t122 * t123 * t4)
      t129 = 0.2D1 * t119 * t127
      t132 = 0.1D1 / t123
      t135 = t2 * t111
      t136 = x2 * x1
      t137 = t136 * z
      t138 = 0.1D1 - x1 + t116 - x2 + t136 - t137 - x3 + t111 - t112 + t
     #114 - t115 + t117 + t129
      t143 = t2 * x1 * t4
      t144 = t1 ** 2
      t150 = t110 ** 2
      t151 = t132 * t150
      t157 = log(0.4D1 * t151 * x2 * t120 * t122 * t92)
      t159 = x3 * t118
      t162 = x2 * z
      t163 = t41 * t127
      t167 = t127 * x1
      t170 = t118 * x2
      t171 = t170 * x3
      t174 = x2 * t41
      t179 = x3 * t72
      t183 = t72 * t170
      t184 = t9 * x3
      t186 = x1 * t170
      t194 = t163 * x1
      t197 = t118 * x1
      t198 = -0.6D1 * t159 * t116 - 0.2D1 * t162 * t163 - 0.2D1 * z * t4
     #1 * t167 + 0.4D1 * t171 * t116 - 0.2D1 * t174 * t167 + t111 * t118
     # * t9 + 0.4D1 * t179 * t118 * z + t183 * t184 - t186 * t184 - 0.2D
     #1 * t183 * x3 * z - 0.2D1 * t72 * t9 * t159 - t170 + 0.2D1 * t162 
     #* t194 + t186 - t197
      t199 = 0.3D1 * t159
      t200 = 0.2D1 * t171
      t201 = t170 * z
      t210 = t159 * z
      t217 = 0.2D1 * t171 * z
      t218 = -t199 + t200 + t201 - 0.2D1 * t163 + t118 - 0.2D1 * t179 * 
     #t118 + t183 * x3 - 0.3D1 * t186 * x3 - t186 * z + t197 * z + t210 
     #+ 0.5D1 * t159 * x1 + 0.2D1 * t194 + 0.2D1 * t174 * t127 - t217
      t220 = (t198 + t218) ** 2
      t223 = (x2 - t136 - 0.1D1 + x1 - t116 - t162 + t137) ** 2
      t225 = t132 * t220 / t223
      t230 = -0.90D2 * t29 * t157 * t225 - 0.180D3 * t48 * t225
      t234 = FJET(XB1, XB2, s, t2 * t110 * (-x3 + t111 - t112 + t114 - t
     #115 + t117 - x2 + t129) * t132, t135, -t2 * t110 * t138 * t132, -t
     #143, -s * t144 * x2 * x1 * t110 * t132, -t230 * t69 * t88 / 0.90D2
     #)
      t241 = Sqrt(t121 * t122 * t4)
      t243 = 0.2D1 * t119 * t241
      t250 = t11 * t120 * t122
      t253 = log(0.4D1 * t113 * t4 * t250)
      t254 = t41 * t241
      t261 = (-t210 + 0.2D1 * t254 + t217 - 0.2D1 * t174 * t241 + t170 -
     # t118 + t199 - t200 - t201 + 0.2D1 * t162 * t254) ** 2
      t264 = (-x2 + 0.1D1 + t162) ** 2
      t265 = 0.1D1 / t264
      t269 = t253 ** 2
      t283 = log(0.4D1 * t73 * t12 * t250)
      t296 = -(-0.180D3 * t48 * t253 * t261 * t265 - 0.45D2 * t29 * t269
     # * t261 * t265 - t29 * t21 * t261 * t265) * t69 / 0.180D3 - (0.90D
     #2 * t29 * t283 * t261 * t265 + 0.180D3 * t29 * lh * t261 * t265) *
     # t69 * t88 / 0.90D2
      t297 = FJET(XB1, XB2, s, -t2 * (-x3 + t114 - x2 + t243), 0.0D0, t2
     # * (0.1D1 - x2 - x3 + t114 + t243), 0.0D0, 0.0D0, t296)
      t304 = t12 * t151
      t307 = log(-0.4D1 * t73 * t11 * t304)
      t311 = Sqrt(-x3 * t123 * t4)
      t312 = t311 ** 2
      t313 = t132 * t312
      t314 = t313 * t42
      t326 = log(-0.4D1 * t91 * t10 * t304)
      t332 = t326 ** 2
      t342 = -(0.360D3 * t29 * t307 * t314 + 0.720D3 * t48 * t314) * t69
     # * t88 / 0.90D2 - 0.2D1 / 0.45D2 * (-0.180D3 * t48 * t42 * t326 * 
     #t313 - 0.45D2 * t29 * t42 * t332 * t132 * t312 - t29 * t21 * t314)
     # * t88
      t343 = FJET(XB1, XB2, s, -t2 * t110 * x3, t135, t2 * t110 * t4, -t
     #143, 0.0D0, t342)
      bbarqqbarH3n1e1 = t108 * t107 - t234 * t230 * t69 * t88 / 0.90D2 +
     # t297 * t296 + t343 * t342

      end function



      doubleprecision function bbarqqbarH3n1e0
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
      t7 = x4 * 0.3141592653589793D1
      t8 = Sin(t7)
      t9 = t8 ** 2
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t14 = t12 * x3 * t4
      t17 = log(-0.4D1 * x2 * t9 * t14)
      t18 = cos(t7)
      t19 = t18 ** 2
      t21 = x3 * t4
      t22 = Sqrt(-t21)
      t23 = t22 ** 2
      t29 = t6 * lh * t19 * t23
      t32 = 0.1D1 / x2
      t35 = t6 * t19
      t37 = 0.1D1 / x1
      t41 = x1 ** 2
      t42 = t41 * t9
      t45 = log(-0.4D1 * t42 * t14)
      t54 = t9 * t12
      t57 = log(-0.4D1 * t54 * t21)
      t62 = lh ** 2
      t64 = 0.3141592653589793D1 ** 2
      t68 = t57 ** 2
      t76 = -(-0.360D3 * t6 * t17 * t19 * t23 - 0.720D3 * t29) * t32 / 0
     #.180D3 - 0.4D1 * t35 * t23 * t32 * t37 - 0.2D1 / 0.45D2 * (-0.90D2
     # * t6 * t19 * t45 * t23 - 0.180D3 * t29) * t37 - (0.180D3 * t57 * 
     #wd * z * lh + t6 * (0.180D3 * t62 - 0.30D2 * t64) + 0.45D2 * t68 *
     # wd * z) * t19 * t23 / 0.45D2
      t77 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t7
     #6)
      t79 = -0.1D1 + x1
      t80 = x3 * x1
      t81 = t80 * z
      t82 = x2 * x3
      t83 = 0.2D1 * t82
      t84 = t82 * x1
      t85 = x1 * z
      t86 = t82 * t85
      t87 = sqrt(x2)
      t88 = t18 * t87
      t89 = -0.1D1 + t87
      t90 = x3 * t89
      t91 = t87 + 0.1D1
      t92 = 0.1D1 - x1 + t85
      t96 = Sqrt(t90 * t91 * t92 * t4)
      t98 = 0.2D1 * t88 * t96
      t101 = 0.1D1 / t92
      t104 = t2 * t80
      t105 = x2 * x1
      t106 = t105 * z
      t107 = 0.1D1 - x1 + t85 - x2 + t105 - t106 - x3 + t80 - t81 + t83 
     #- t84 + t86 + t98
      t112 = t2 * x1 * t4
      t113 = t1 ** 2
      t119 = t6 * t101
      t120 = x2 * z
      t121 = t18 * t96
      t122 = t121 * x1
      t125 = x3 * t41
      t128 = t87 * x2
      t129 = t41 * t128
      t131 = x1 * t128
      t135 = t87 * x1
      t137 = x3 * t87
      t138 = t137 * z
      t142 = x2 * t18
      t145 = t128 * x3
      t147 = 0.2D1 * t145 * z
      t148 = 0.3D1 * t137
      t149 = 0.2D1 * t145
      t150 = 0.2D1 * t120 * t122 - 0.2D1 * t125 * t87 + t129 * x3 - 0.3D
     #1 * t131 * x3 - t131 * z + t135 * z + t138 + 0.5D1 * t137 * x1 + 0
     #.2D1 * t122 + 0.2D1 * t142 * t96 - t147 + t131 - t135 - t148 + t14
     #9
      t151 = t128 * z
      t158 = t96 * x1
      t170 = t11 * x3
      t179 = t151 - 0.2D1 * t121 - 0.6D1 * t137 * t85 - 0.2D1 * t120 * t
     #121 - 0.2D1 * z * t18 * t158 + 0.4D1 * t145 * t85 - 0.2D1 * t142 *
     # t158 + t80 * t87 * t11 + 0.4D1 * t125 * t87 * z + t129 * t170 - t
     #131 * t170 - 0.2D1 * t129 * x3 * z - 0.2D1 * t41 * t11 * t137 + t8
     #7 - t128
      t181 = (t150 + t179) ** 2
      t183 = (x2 - t105 - 0.1D1 + x1 - t85 - t120 + t106) ** 2
      t186 = t32 * t37
      t187 = t181 / t183 * t186
      t189 = FJET(XB1, XB2, s, t2 * t79 * (-x3 + t80 - t81 + t83 - t84 +
     # t86 - x2 + t98) * t101, t104, -t2 * t79 * t107 * t101, -t112, -s 
     #* t113 * x2 * x1 * t79 * t101, -t119 * t187)
      t196 = Sqrt(t90 * t91 * t4)
      t198 = 0.2D1 * t88 * t196
      t208 = log(0.4D1 * t82 * t4 * t54 * t89 * t91)
      t209 = t18 * t196
      t216 = (-t138 + 0.2D1 * t209 + t147 - 0.2D1 * t142 * t196 + t128 -
     # t87 + t148 - t149 - t151 + 0.2D1 * t120 * t209) ** 2
      t219 = (-x2 + 0.1D1 + t120) ** 2
      t220 = 0.1D1 / t219
      t235 = -(0.90D2 * t6 * t208 * t216 * t220 + 0.180D3 * t6 * lh * t2
     #16 * t220) * t32 / 0.180D3 + t6 * t216 * t220 * t32 * t37
      t236 = FJET(XB1, XB2, s, -t2 * (-x3 + t83 - x2 + t198), 0.0D0, t2 
     #* (0.1D1 - x2 - x3 + t83 + t198), 0.0D0, 0.0D0, t235)
      t244 = Sqrt(-x3 * t92 * t4)
      t245 = t244 ** 2
      t251 = t79 ** 2
      t256 = log(-0.4D1 * t42 * t12 * t21 * t101 * t251)
      t269 = 0.4D1 * t119 * t245 * t19 * t186 - 0.2D1 / 0.45D2 * (0.90D2
     # * t35 * t256 * t101 * t245 + 0.180D3 * t6 * lh * t101 * t245 * t1
     #9) * t37
      t270 = FJET(XB1, XB2, s, -t2 * t79 * x3, t104, t2 * t79 * t4, -t11
     #2, 0.0D0, t269)
      bbarqqbarH3n1e0 = t77 * t76 - t189 * wd * z * t101 * t187 + t236 *
     # t235 + t270 * t269

      end function



      doubleprecision function bbarqqbarH3n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t2 = (-0.1D1 + z) * s
      t4 = -0.1D1 + x3
      t6 = wd * z
      t9 = x4 * 0.3141592653589793D1
      t10 = Sin(t9)
      t11 = t10 ** 2
      t12 = z ** 2
      t15 = x3 * t4
      t18 = log(-0.4D1 * t11 / t12 * t15)
      t23 = cos(t9)
      t24 = t23 ** 2
      t26 = Sqrt(-t15)
      t27 = t26 ** 2
      t30 = t24 * t27
      t31 = 0.1D1 / x2
      t35 = 0.1D1 / x1
      t39 = -(-0.180D3 * t6 * lh - 0.90D2 * t18 * wd * z) * t24 * t27 / 
     #0.45D2 - 0.2D1 * t6 * t30 * t31 - 0.4D1 * t6 * t30 * t35
      t40 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t3
     #9)
      t43 = 0.2D1 * x2 * x3
      t44 = sqrt(x2)
      t51 = Sqrt(x3 * (-0.1D1 + t44) * (t44 + 0.1D1) * t4)
      t53 = 0.2D1 * t23 * t44 * t51
      t58 = x3 * t44
      t60 = t23 * t51
      t62 = t44 * x2
      t63 = t62 * x3
      t72 = x2 * z
      t76 = (-t58 * z + 0.2D1 * t60 + 0.2D1 * t63 * z - 0.2D1 * x2 * t23
     # * t51 + t62 - t44 + 0.3D1 * t58 - 0.2D1 * t63 - t62 * z + 0.2D1 *
     # t72 * t60) ** 2
      t78 = (-x2 + 0.1D1 + t72) ** 2
      t81 = t76 / t78 * t31
      t84 = FJET(XB1, XB2, s, -t2 * (-x3 + t43 - x2 + t53), 0.0D0, t2 * 
     #(0.1D1 - x2 - x3 + t43 + t53), 0.0D0, 0.0D0, t6 * t81 / 0.2D1)
      t89 = -0.1D1 + x1
      t99 = 0.1D1 - x1 + x1 * z
      t100 = 0.1D1 / t99
      t104 = Sqrt(-x3 * t99 * t4)
      t105 = t104 ** 2
      t110 = FJET(XB1, XB2, s, -t2 * t89 * x3, t2 * x1 * x3, t2 * t89 * 
     #t4, -t2 * x1 * t4, 0.0D0, 0.4D1 * t6 * t100 * t105 * t24 * t35)
      bbarqqbarH3n1em1 = t40 * t39 + t84 * wd * z * t81 / 0.2D1 + 0.4D1 
     #* t110 * wd * z * t100 * t105 * t24 * t35

      end function



      doubleprecision function bbarqqbarH3n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t2 = (-0.1D1 + z) * s
      t4 = -0.1D1 + x3
      t8 = cos(x4 * 0.3141592653589793D1)
      t9 = t8 ** 2
      t11 = Sqrt(-x3 * t4)
      t12 = t11 ** 2
      t16 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, -0
     #.2D1 * wd * z * t9 * t12)
      bbarqqbarH3n1em2 = -0.2D1 * t16 * wd * z * t9 * t12

      end function



      doubleprecision function bbarqqbarH3n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      bbarqqbarH3n1em3 = 0.0D0

      end function



      doubleprecision function bbarqqbarH3n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      bbarqqbarH3n1em4 = 0.0D0

      end function


      doubleprecision function bbarqqbarH3n2e1
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
      t13 = x3 * t4
      t16 = log(-0.4D1 * t12 * t13)
      t17 = x3 * z
      t19 = Sqrt(-t17 * t4)
      t20 = t19 ** 2
      t22 = cos(t9)
      t23 = t22 ** 2
      t24 = wd * t23
      t25 = lh ** 2
      t27 = 0.3141592653589793D1 ** 2
      t29 = 0.180D3 * t25 - 0.30D2 * t27
      t30 = t24 * t29
      t33 = t16 ** 2
      t35 = t24 * lh
      t38 = t20 * wd
      t51 = wd * lh
      t52 = x2 * t8
      t55 = sqrt(x2)
      t56 = -0.1D1 + t55
      t57 = t55 + 0.1D1
      t58 = t56 * t57
      t62 = log(0.4D1 * t52 * x3 * t4 * t11 * t58)
      t63 = x3 * t55
      t64 = t63 * z
      t65 = x3 * t56
      t69 = Sqrt(t65 * t57 * z * t4)
      t73 = (t64 + 0.2D1 * t22 * t69 - t55 + t63) ** 2
      t79 = log(-0.4D1 * t52 * t11 * x3 * t4)
      t86 = t79 ** 2
      t90 = t62 ** 2
      t99 = t73 - 0.4D1 * t20 * t23
      t102 = 0.1D1 / x2
      t105 = x1 ** 2
      t106 = x2 * t105
      t111 = log(0.4D1 * t106 * t13 * t12 * t58)
      t115 = t8 * x3 * t4
      t118 = log(-0.4D1 * t106 * t11 * t115)
      t130 = 0.1D1 / x1
      t133 = t105 * t11
      t136 = log(-0.4D1 * t133 * t115)
      t141 = t136 ** 2
      t151 = t16 * t20 * t30 / 0.45D2 + 0.2D1 * t33 * t20 * t35 - t38 * 
     #t23 * (0.60D2 * lh * t27 - 0.2884936567583026D3 - 0.120D3 * t25 * 
     #lh) / 0.45D2 + t33 * t16 * t20 * t24 / 0.3D1 + (-0.180D3 * t51 * (
     #-t62 * t73 + 0.4D1 * t79 * t20 * t23) + 0.90D2 * wd * (-0.2D1 * t8
     #6 * t20 * t23 + t90 * t73 / 0.2D1) + wd * t29 * t99) * t102 / 0.18
     #0D3 - (0.90D2 * wd * (t111 * t73 - 0.4D1 * t118 * t20 * t23) + 0.1
     #80D3 * t51 * t99) * t102 * t130 / 0.90D2 + (-0.720D3 * t24 * lh * 
     #t136 * t20 - 0.180D3 * t24 * t141 * t20 - 0.4D1 * t38 * t23 * t29)
     # * t130 / 0.90D2
      t152 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t
     #151)
      t154 = x3 * x1
      t156 = -0.1D1 + x1
      t158 = t2 * t156 * x3
      t162 = t2 * t156 * t4
      t163 = 0.1D1 / t6
      t166 = x1 * z
      t167 = -z - x1 + t166
      t168 = 0.1D1 / t167
      t169 = t156 ** 2
      t171 = t13 * t168 * t169
      t174 = log(0.4D1 * t106 * t11 * t163 * t171)
      t178 = Sqrt(x3 * t167 * t4)
      t179 = t178 ** 2
      t181 = t168 * z
      t182 = t181 * t23
      t194 = log(0.4D1 * t133 * t163 * t171)
      t199 = t194 ** 2
      t202 = t179 * t168 * z
      t210 = -(-0.360D3 * wd * t174 * t179 * t182 - 0.720D3 * t51 * t179
     # * t182) * t102 * t130 / 0.90D2 + (-0.720D3 * t35 * t194 * t179 * 
     #t181 - 0.180D3 * t24 * t199 * t202 - 0.4D1 * t30 * t202) * t130 / 
     #0.90D2
      t211 = FJET(XB1, XB2, s, t2 * t154, -t158, -t2 * x1 * t4, t162, 0.
     #0D0, t210)
      t213 = t154 * z
      t214 = x2 * x3
      t215 = t214 * z
      t216 = t214 * x1
      t217 = t214 * t166
      t222 = Sqrt(-t65 * t57 * t167 * t4)
      t224 = 0.2D1 * t22 * t55 * t222
      t230 = x2 * x1
      t231 = t230 * z
      t232 = z + x1 - t166 - x2 * z - t230 + t231 - t17 - t154 + t213 + 
     #t215 + t216 - t217 + t214 + t224
      t236 = t1 ** 2
      t250 = log(-0.4D1 * t106 * t13 * t163 * t11 * t168 * t169 * t56 * 
     #t57)
      t256 = z * t22 * t222
      t261 = x3 * t105
      t262 = t55 * z
      t271 = t55 * x2
      t272 = t105 * t271
      t275 = t22 * t222
      t278 = x1 * t271
      t279 = t6 * x3
      t282 = 0.2D1 * t63 * t166 + 0.2D1 * t256 - 0.3D1 * t154 * t55 * t6
     # - 0.4D1 * t261 * t262 + 0.2D1 * t105 * t6 * t63 + 0.2D1 * x1 * t2
     #2 * t222 + 0.2D1 * t272 * t17 - 0.2D1 * t230 * t275 + t278 * t279 
     #- t272 * t279 + t278
      t283 = t55 * x1
      t296 = -t262 - t283 + 0.2D1 * t261 * t55 - t272 * x3 - t278 * x3 -
     # t278 * z + t63 * t6 + t283 * z + t64 + t63 * x1 - 0.2D1 * t166 * 
     #t275 + 0.2D1 * t230 * t256
      t298 = (t282 + t296) ** 2
      t301 = (z - t230 + x1 - t166 + t231) ** 2
      t303 = t168 * t298 / t301
      t309 = 0.90D2 * wd * t250 * z * t303 + 0.180D3 * t51 * z * t303
      t313 = FJET(XB1, XB2, s, t2 * x1 * (-t17 - t154 + t213 + t215 + t2
     #16 - t217 - x2 + t214 + t224) * t168, -t158, -t2 * x1 * t232 * t16
     #8, t162, s * t236 * x2 * x1 * t156 * t168, -t309 * t102 * t130 / 0
     #.90D2)
      bbarqqbarH3n2e1 = t152 * t151 + t211 * t210 - t313 * t309 * t102 *
     # t130 / 0.90D2

      end function



      doubleprecision function bbarqqbarH3n2e0
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
      t9 = x2 * t8
      t11 = x4 * 0.3141592653589793D1
      t12 = Sin(t11)
      t13 = t12 ** 2
      t15 = sqrt(x2)
      t16 = -0.1D1 + t15
      t17 = t15 + 0.1D1
      t22 = log(0.4D1 * t9 * x3 * t4 * t13 * t16 * t17)
      t23 = x3 * t15
      t24 = t23 * z
      t25 = cos(t11)
      t26 = x3 * t16
      t30 = Sqrt(t26 * t17 * z * t4)
      t34 = (t24 + 0.2D1 * t25 * t30 - t15 + t23) ** 2
      t40 = log(-0.4D1 * t9 * t13 * x3 * t4)
      t41 = x3 * z
      t43 = Sqrt(-t41 * t4)
      t44 = t43 ** 2
      t46 = t25 ** 2
      t52 = wd * lh
      t55 = t34 - 0.4D1 * t44 * t46
      t59 = 0.1D1 / x2
      t64 = 0.1D1 / x1
      t65 = t59 * t64
      t67 = wd * t46
      t68 = x1 ** 2
      t69 = t68 * t13
      t74 = log(-0.4D1 * t69 * t8 * x3 * t4)
      t78 = t44 * wd
      t86 = x3 * t4
      t89 = log(-0.4D1 * t8 * t13 * t86)
      t94 = lh ** 2
      t96 = 0.3141592653589793D1 ** 2
      t102 = t89 ** 2
      t105 = (0.90D2 * wd * (-t22 * t34 + 0.4D1 * t40 * t44 * t46) - 0.1
     #80D3 * t52 * t55) * t59 / 0.180D3 + wd * t55 * t65 + (0.360D3 * t6
     #7 * t74 * t44 + 0.720D3 * t78 * t46 * lh) * t64 / 0.90D2 - 0.4D1 *
     # t89 * t44 * t67 * lh - t78 * t46 * (0.180D3 * t94 - 0.30D2 * t96)
     # / 0.45D2 - t102 * t44 * t67
      t106 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t
     #105)
      t108 = x3 * x1
      t110 = -0.1D1 + x1
      t112 = t2 * t110 * x3
      t116 = t2 * t110 * t4
      t117 = x1 * z
      t118 = -z - x1 + t117
      t121 = Sqrt(x3 * t118 * t4)
      t122 = t121 ** 2
      t124 = 0.1D1 / t118
      t132 = t110 ** 2
      t137 = log(0.4D1 * t69 / t6 * t86 * t124 * t132)
      t144 = t124 * z
      t151 = -0.4D1 * wd * t122 * t124 * z * t46 * t65 + (0.360D3 * t67 
     #* t137 * t122 * t124 * z + 0.720D3 * t52 * t122 * t144 * t46) * t6
     #4 / 0.90D2
      t152 = FJET(XB1, XB2, s, t2 * t108, -t112, -t2 * x1 * t4, t116, 0.
     #0D0, t151)
      t154 = t108 * z
      t155 = x2 * x3
      t156 = t155 * z
      t157 = t155 * x1
      t158 = t155 * t117
      t163 = Sqrt(-t26 * t17 * t118 * t4)
      t165 = 0.2D1 * t25 * t15 * t163
      t171 = x2 * x1
      t172 = t171 * z
      t173 = z + x1 - t117 - x2 * z - t171 + t172 - t41 - t108 + t154 + 
     #t156 + t157 - t158 + t155 + t165
      t177 = t1 ** 2
      t188 = z * t25 * t163
      t193 = x3 * t68
      t194 = t15 * z
      t203 = t15 * x2
      t204 = t68 * t203
      t207 = t25 * t163
      t210 = x1 * t203
      t211 = t6 * x3
      t214 = 0.2D1 * t23 * t117 + 0.2D1 * t188 - 0.3D1 * t108 * t15 * t6
     # - 0.4D1 * t193 * t194 + 0.2D1 * t68 * t6 * t23 + 0.2D1 * x1 * t25
     # * t163 + 0.2D1 * t204 * t41 - 0.2D1 * t171 * t207 + t211 * t210 -
     # t204 * t211 + t210
      t215 = t15 * x1
      t228 = -t194 - t215 + 0.2D1 * t193 * t15 - t204 * x3 - t210 * x3 -
     # t210 * z + t23 * t6 + t215 * z + t24 + t23 * x1 - 0.2D1 * t117 * 
     #t207 + 0.2D1 * t171 * t188
      t230 = (t214 + t228) ** 2
      t232 = (z - t171 + x1 - t117 + t172) ** 2
      t235 = t230 / t232 * t65
      t237 = FJET(XB1, XB2, s, t2 * x1 * (-t41 - t108 + t154 + t156 + t1
     #57 - t158 - x2 + t155 + t165) * t124, -t112, -t2 * x1 * t173 * t12
     #4, t116, s * t177 * x2 * x1 * t110 * t124, wd * z * t124 * t235)
      bbarqqbarH3n2e0 = t106 * t105 + t152 * t151 + t237 * wd * t144 * t
     #235

      end function



      doubleprecision function bbarqqbarH3n2em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t2 = s * (-0.1D1 + z)
      t4 = -0.1D1 + x3
      t8 = Sqrt(-x3 * z * t4)
      t9 = t8 ** 2
      t11 = x4 * 0.3141592653589793D1
      t12 = cos(t11)
      t13 = t12 ** 2
      t17 = z ** 2
      t20 = Sin(t11)
      t21 = t20 ** 2
      t26 = log(-0.4D1 / t17 / z * t21 * x3 * t4)
      t28 = wd * t13
      t31 = sqrt(x2)
      t32 = x3 * t31
      t40 = Sqrt(x3 * (-0.1D1 + t31) * (t31 + 0.1D1) * z * t4)
      t44 = (t32 * z + 0.2D1 * t12 * t40 - t31 + t32) ** 2
      t52 = 0.1D1 / x1
      t56 = 0.4D1 * t9 * wd * t13 * lh + 0.2D1 * t26 * t9 * t28 + wd * (
     #t44 - 0.4D1 * t9 * t13) / x2 / 0.2D1 - 0.4D1 * t28 * t9 * t52
      t57 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t5
     #6)
      t61 = -0.1D1 + x1
      t69 = -z - x1 + x1 * z
      t72 = Sqrt(x3 * t69 * t4)
      t73 = t72 ** 2
      t75 = 0.1D1 / t69
      t80 = FJET(XB1, XB2, s, t2 * x1 * x3, -t2 * t61 * x3, -t2 * x1 * t
     #4, t2 * t61 * t4, 0.0D0, -0.4D1 * t28 * t73 * t75 * z * t52)
      bbarqqbarH3n2em1 = t56 * t57 - 0.4D1 * t80 * wd * t13 * t73 * t75 
     #* z * t52

      end function



      doubleprecision function bbarqqbarH3n2em2
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
     #.2D1 * wd * t8 * t13)
      bbarqqbarH3n2em2 = -0.2D1 * t16 * wd * t8 * t13
	  !print*,z,x3,x4,-0.2D1  * wd * t8 * t13
      end function



      doubleprecision function bbarqqbarH3n2em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      bbarqqbarH3n2em3 = 0.0D0

      end function



      doubleprecision function bbarqqbarH3n2em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      bbarqqbarH3n2em4 = 0.0D0

      end function
