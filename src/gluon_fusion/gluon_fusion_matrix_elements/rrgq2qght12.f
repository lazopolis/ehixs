  
      subroutine rrgq2qght12
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgq2qght12s1e1  
      doubleprecision rrgq2qght12s1e0  
      doubleprecision rrgq2qght12s1em1  
      doubleprecision rrgq2qght12s1em2  
      doubleprecision rrgq2qght12s1em3  
      doubleprecision rrgq2qght12s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght12s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgq2qght12s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght12s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgq2qght12s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgq2qght12s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgq2qght12s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgq2qght12s1e1
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
      t3 = x2 * x3
      t4 = 0.2D1 * t3
      t5 = x4 * 0.3141592653589793D1
      t6 = cos(t5)
      t7 = -0.1D1 + x2
      t8 = x3 * t7
      t9 = -0.1D1 + x3
      t12 = Sqrt(t8 * x2 * t9)
      t14 = 0.2D1 * t6 * t12
      t19 = z * wd
      t20 = x2 * z
      t22 = (0.1D1 - x2 + t20) ** 2
      t23 = 0.1D1 / t22
      t24 = t19 * t23
      t25 = Sin(t5)
      t26 = t25 ** 2
      t27 = t3 * t26
      t28 = z ** 2
      t29 = 0.1D1 / t28
      t30 = t1 ** 2
      t31 = t30 ** 2
      t32 = t29 * t31
      t34 = t32 * t7 * t9
      t37 = log(0.4D1 * t27 * t34)
      t40 = t37 ** 2
      t42 = lh ** 2
      t43 = 0.180D3 * t42
      t44 = 0.3141592653589793D1 ** 2
      t45 = 0.30D2 * t44
      t48 = -x2 + t20 - 0.1D1 + x3
      t51 = 0.180D3 * lh
      t58 = 0.1D1 / x3
      t61 = t19 * t1
      t62 = x1 ** 2
      t63 = t62 * t26
      t64 = t3 * t63
      t67 = log(0.4D1 * t64 * t34)
      t72 = 0.90D2 * t19
      t73 = lh * z
      t76 = (0.180D3 * z + 0.180D3 * t73) * wd
      t78 = (-t72 + t76) * t1
      t83 = 0.1D1 / x1
      t86 = (t24 * t1 * (-0.180D3 * t37 * lh - 0.45D2 * t40 - t43 + t45)
     # * t48 - t24 * t1 * (t51 + 0.90D2 * t37) * t48) * t58 / 0.810D3 - 
     #(-0.90D2 * t61 * t67 * t23 * t48 - t78 * t23 * t48) * t58 * t83 / 
     #0.405D3
      t87 = FJET(XB1, XB2, s, t2 * (0.1D1 - x2 - x3 + t4 + t14), 0.0D0, 
     #-t2 * (-x3 + t4 - x2 + t14), 0.0D0, 0.0D0, t86)
      t90 = -0.1D1 + x1
      t95 = x1 * z
      t96 = 0.1D1 - x1 + t95
      t97 = 0.1D1 / t96
      t104 = s * t30 * x2 * x1 * t90 * t97
      t105 = t90 ** 2
      t111 = log(-0.4D1 * t64 * t32 * t97 * t105 * t7)
      t113 = x2 * x1
      t114 = t113 * z
      t116 = (-0.1D1 + x1 - t95 + x2 - t113 - t20 + t114) ** 2
      t117 = 0.1D1 / t116
      t118 = x2 - t113 - t20 + t114 + 0.1D1 - x1 + t95
      t119 = t117 * t118
      t123 = t90 * t117
      t124 = t123 * t118
      t129 = x2 * t62
      t130 = t26 * t29
      t132 = t31 * t97
      t133 = t105 * t7
      t137 = log(-0.4D1 * t129 * t130 * t132 * t133)
      t138 = t137 ** 2
      t145 = 0.90D2 * z
      t147 = -t43 + t45
      t152 = (-t72 + t76 + (-t145 - 0.360D3 * t73 + t147 * z) * wd) * t1
      t157 = -(-0.90D2 * t61 * t111 * t90 * t119 - t78 * t124) * t58 * t
     #83 / 0.405D3 + (-0.45D2 * t61 * t138 * t90 * t119 - t78 * t137 * t
     #124 + t152 * t124) * t83 / 0.405D3
      t158 = FJET(XB1, XB2, s, t7 * s * t1 * t90, t2 * x1, -t2 * t90 * x
     #2 * t97, 0.0D0, -t104, t157)
      t162 = t32 * t7
      t165 = log(-0.4D1 * t27 * t162)
      t168 = t165 ** 2
      t172 = -x2 + t20 - 0.1D1
      t187 = log(-0.4D1 * x2 * t26 * t162)
      t190 = (-t51 - 0.90D2 * t187) * z
      t196 = t187 ** 2
      t199 = (0.180D3 * t187 * lh + 0.45D2 * t196 + t43 - t45) * z
      t227 = log(-0.4D1 * t3 * t62 * t130 * t31 * t7)
      t232 = t23 * t172
      t241 = log(-0.4D1 * t129 * t26 * t162)
      t242 = t241 ** 2
      t254 = (t24 * t1 * (0.180D3 * t165 * lh + 0.45D2 * t168 + t43 - t4
     #5) * t172 - t24 * t1 * (-t51 - 0.90D2 * t165) * t172) * t58 / 0.81
     #0D3 + (t72 + (-0.180D3 * z + t190) * wd + (t145 - 0.2D1 * t190 + t
     #199) * wd + (t190 - 0.2D1 * t199 + (-0.90D2 * t196 * lh + 0.60D2 *
     # lh * t44 - 0.2884936567583026D3 - 0.120D3 * t42 * lh - 0.15D2 * t
     #196 * t187 + t187 * t147) * z) * wd) * t23 * t172 * t1 / 0.810D3 -
     # (0.90D2 * t61 * t227 * t23 * t172 + t78 * t232) * t58 * t83 / 0.4
     #05D3 + (0.45D2 * t61 * t242 * t23 * t172 + t78 * t241 * t23 * t172
     # - t152 * t232) * t83 / 0.405D3
      t255 = FJET(XB1, XB2, s, -t2 * t7, 0.0D0, t2 * x2, 0.0D0, 0.0D0, t
     #254)
      t257 = x3 * x1
      t258 = t257 * z
      t259 = t3 * x1
      t260 = t3 * t95
      t264 = Sqrt(t8 * t96 * x2 * t9)
      t266 = 0.2D1 * t6 * t264
      t267 = 0.1D1 - x1 + t95 - x2 + t113 - t114 - x3 + t257 - t258 + t4
     # - t259 + t260 + t266
      t285 = log(0.4D1 * t3 * t63 * t29 * t132 * t133 * t9)
      t287 = x2 - t113 - t20 + t114 + 0.1D1 - x1 + t95 - x3 + t257 - t25
     #8
      t294 = 0.90D2 * t61 * t285 * t90 * t117 * t287 + t78 * t123 * t287
      t298 = FJET(XB1, XB2, s, -t2 * t90 * t267 * t97, -t9 * s * t1 * x1
     #, t2 * t90 * (-x3 + t257 - t258 + t4 - t259 + t260 - x2 + t266) * 
     #t97, t2 * t257, -t104, -t294 * t58 * t83 / 0.405D3)
      rrgq2qght12s1e1 = t87 * t86 + t158 * t157 + t255 * t254 - t298 * t
     #294 * t58 * t83 / 0.405D3

      end function



      doubleprecision function rrgq2qght12s1e0
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
      t3 = x2 * x3
      t4 = 0.2D1 * t3
      t5 = x4 * 0.3141592653589793D1
      t6 = cos(t5)
      t7 = -0.1D1 + x2
      t8 = x3 * t7
      t9 = -0.1D1 + x3
      t12 = Sqrt(t8 * x2 * t9)
      t14 = 0.2D1 * t6 * t12
      t19 = z * wd
      t20 = x2 * z
      t22 = (0.1D1 - x2 + t20) ** 2
      t23 = 0.1D1 / t22
      t24 = t19 * t23
      t25 = 0.180D3 * lh
      t26 = Sin(t5)
      t27 = t26 ** 2
      t28 = t3 * t27
      t29 = z ** 2
      t30 = 0.1D1 / t29
      t31 = t1 ** 2
      t32 = t31 ** 2
      t33 = t30 * t32
      t38 = log(0.4D1 * t28 * t33 * t7 * t9)
      t42 = -x2 + t20 - 0.1D1 + x3
      t45 = t23 * t1
      t51 = 0.1D1 / x3
      t54 = t19 * t1
      t56 = 0.1D1 / x1
      t57 = t51 * t56
      t61 = (t24 * t1 * (t25 + 0.90D2 * t38) * t42 + 0.90D2 * t19 * t45 
     #* t42) * t51 / 0.810D3 - 0.2D1 / 0.9D1 * t54 * t23 * t42 * t57
      t62 = FJET(XB1, XB2, s, t2 * (0.1D1 - x2 - x3 + t4 + t14), 0.0D0, 
     #-t2 * (-x3 + t4 - x2 + t14), 0.0D0, 0.0D0, t61)
      t65 = -0.1D1 + x1
      t66 = t1 * t65
      t70 = x1 * z
      t71 = 0.1D1 - x1 + t70
      t72 = 0.1D1 / t71
      t79 = s * t31 * x2 * x1 * t65 * t72
      t80 = t19 * t66
      t81 = x2 * x1
      t82 = t81 * z
      t84 = (-0.1D1 + x1 - t70 + x2 - t81 - t20 + t82) ** 2
      t85 = 0.1D1 / t84
      t86 = x2 - t81 - t20 + t82 + 0.1D1 - x1 + t70
      t87 = t85 * t86
      t91 = x1 ** 2
      t92 = x2 * t91
      t96 = t65 ** 2
      t101 = log(-0.4D1 * t92 * t27 * t30 * t32 * t72 * t96 * t7)
      t106 = 0.90D2 * t19
      t112 = (-t106 + (0.180D3 * z + 0.180D3 * lh * z) * wd) * t1
      t113 = t65 * t85
      t119 = -0.2D1 / 0.9D1 * t80 * t87 * t57 + (0.90D2 * t54 * t101 * t
     #65 * t87 + t112 * t113 * t86) * t56 / 0.405D3
      t120 = FJET(XB1, XB2, s, t7 * s * t66, t2 * x1, -t2 * t65 * x2 * t
     #72, 0.0D0, -t79, t119)
      t124 = t33 * t7
      t127 = log(-0.4D1 * t28 * t124)
      t131 = -x2 + t20 - 0.1D1
      t140 = t1 * t131
      t147 = log(-0.4D1 * t92 * t27 * t124)
      t161 = log(-0.4D1 * x2 * t27 * t124)
      t164 = (-t25 - 0.90D2 * t161) * z
      t171 = t161 ** 2
      t173 = lh ** 2
      t175 = 0.3141592653589793D1 ** 2
      t185 = (t24 * t1 * (-t25 - 0.90D2 * t127) * t131 - 0.90D2 * t19 * 
     #t45 * t131) * t51 / 0.810D3 + 0.2D1 / 0.9D1 * t24 * t140 * t57 + (
     #-0.90D2 * t54 * t147 * t23 * t131 - t112 * t23 * t131) * t56 / 0.4
     #05D3 + (t106 + (-0.180D3 * z + t164) * wd + (0.90D2 * z - 0.2D1 * 
     #t164 + (0.180D3 * t161 * lh + 0.45D2 * t171 + 0.180D3 * t173 - 0.3
     #0D2 * t175) * z) * wd) * t23 * t140 / 0.810D3
      t186 = FJET(XB1, XB2, s, -t2 * t7, 0.0D0, t2 * x2, 0.0D0, 0.0D0, t
     #185)
      t188 = x3 * x1
      t189 = t188 * z
      t190 = t3 * x1
      t191 = t3 * t70
      t195 = Sqrt(t8 * t71 * x2 * t9)
      t197 = 0.2D1 * t6 * t195
      t198 = 0.1D1 - x1 + t70 - x2 + t81 - t82 - x3 + t188 - t189 + t4 -
     # t190 + t191 + t197
      t210 = x2 - t81 - t20 + t82 + 0.1D1 - x1 + t70 - x3 + t188 - t189
      t215 = FJET(XB1, XB2, s, -t2 * t65 * t198 * t72, -t9 * s * t1 * x1
     #, t2 * t65 * (-x3 + t188 - t189 + t4 - t190 + t191 - x2 + t197) * 
     #t72, t2 * t188, -t79, 0.2D1 / 0.9D1 * t80 * t85 * t210 * t57)
      rrgq2qght12s1e0 = t62 * t61 + t120 * t119 + t186 * t185 + 0.2D1 / 
     #0.9D1 * t215 * z * wd * t1 * t113 * t210 * t51 * t56

      end function



      doubleprecision function rrgq2qght12s1em1
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
      t4 = 0.2D1 * x2 * x3
      t5 = x4 * 0.3141592653589793D1
      t6 = cos(t5)
      t7 = -0.1D1 + x2
      t12 = Sqrt(x3 * t7 * x2 * (-0.1D1 + x3))
      t14 = 0.2D1 * t6 * t12
      t19 = z * wd
      t20 = x2 * z
      t22 = (0.1D1 - x2 + t20) ** 2
      t23 = 0.1D1 / t22
      t24 = t19 * t23
      t26 = 0.90D2 * x2 - 0.90D2 * t20 + 0.90D2 - 0.90D2 * x3
      t28 = 0.1D1 / x3
      t32 = FJET(XB1, XB2, s, t2 * (0.1D1 - x2 - x3 + t4 + t14), 0.0D0, 
     #-t2 * (-x3 + t4 - x2 + t14), 0.0D0, 0.0D0, t24 * t1 * t26 * t28 / 
     #0.810D3)
      t41 = -0.1D1 + x1
      t46 = x1 * z
      t48 = 0.1D1 / (0.1D1 - x1 + t46)
      t51 = t1 ** 2
      t58 = x2 * x1
      t59 = t58 * z
      t61 = (-0.1D1 + x1 - t46 + x2 - t58 - t20 + t59) ** 2
      t65 = 0.1D1 / x1
      t67 = t41 / t61 * (x2 - t58 - t20 + t59 + 0.1D1 - x1 + t46) * t65
      t70 = FJET(XB1, XB2, s, t7 * s * t1 * t41, t2 * x1, -t2 * t41 * x2
     # * t48, 0.0D0, -s * t51 * x2 * x1 * t41 * t48, -0.2D1 / 0.9D1 * t1
     #9 * t1 * t67)
      t79 = t1 * (-x2 + t20 - 0.1D1)
      t86 = Sin(t5)
      t87 = t86 ** 2
      t89 = z ** 2
      t91 = t51 ** 2
      t96 = log(-0.4D1 * x2 * t87 / t89 * t91 * t7)
      t109 = 0.2D1 / 0.9D1 * t24 * t79 * t65 + (0.90D2 * t19 + (-0.180D3
     # * z + (-0.180D3 * lh - 0.90D2 * t96) * z) * wd) * t23 * t79 / 0.8
     #10D3 + t24 * t79 * t28 / 0.9D1
      t110 = FJET(XB1, XB2, s, -t2 * t7, 0.0D0, t2 * x2, 0.0D0, 0.0D0, t
     #109)
      rrgq2qght12s1em1 = t32 * z * wd * t23 * t1 * t26 * t28 / 0.810D3 -
     # 0.2D1 / 0.9D1 * t70 * z * wd * t1 * t67 + t110 * t109

      end function



      doubleprecision function rrgq2qght12s1em2
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
      t7 = x2 * z
      t9 = (0.1D1 - x2 + t7) ** 2
      t13 = 0.1D1 / t9 * t1 * (-x2 + t7 - 0.1D1)
      t16 = FJET(XB1, XB2, s, -t2 * (-0.1D1 + x2), 0.0D0, t2 * x2, 0.0D0
     #, 0.0D0, z * wd * t13 / 0.9D1)
      rrgq2qght12s1em2 = t16 * z * wd * t13 / 0.9D1

      end function



      doubleprecision function rrgq2qght12s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgq2qght12s1em3 = 0.0D0

      end function



      doubleprecision function rrgq2qght12s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgq2qght12s1em4 = 0.0D0

      end function
