  
      subroutine bbarbbarH1n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision bbarbbarH11J1  
      doubleprecision bbarbbarH11J2  
      doubleprecision bbarbbarH11J3  
      doubleprecision bbarbbarH1n1e1  
      doubleprecision bbarbbarH1n1e0  
      doubleprecision bbarbbarH1n1em1  
      doubleprecision bbarbbarH1n1em2  
      doubleprecision bbarbbarH1n1em3  
      doubleprecision bbarbbarH1n1em4  
      doubleprecision bbarbbarH1n2e1  
      doubleprecision bbarbbarH1n2e0  
      doubleprecision bbarbbarH1n2em1  
      doubleprecision bbarbbarH1n2em2  
      doubleprecision bbarbbarH1n2em3  
      doubleprecision bbarbbarH1n2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=bbarbbarH1n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarbbarH1n2e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=bbarbbarH1n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarbbarH1n2e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=bbarbbarH1n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarbbarH1n2em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=bbarbbarH1n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarbbarH1n2em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=bbarbbarH1n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarbbarH1n2em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=bbarbbarH1n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarbbarH1n2em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function bbarbbarH1n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH11J1
      doubleprecision bbarbbarH11J2
      doubleprecision bbarbbarH11J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t7 = x4 * 0.3141592653589793D1
      t8 = Sin(t7)
      t9 = t8 ** 2
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t16 = log(-0.4D1 * x3 * t9 * t12 * t4)
      t19 = s ** 2
      t20 = 0.1D1 / t19
      t22 = bbarbbarH11J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t27 = lh ** 2
      t28 = 0.180D3 * t27
      t29 = 0.3141592653589793D1 ** 2
      t30 = 0.30D2 * t29
      t31 = t16 ** 2
      t35 = bbarbbarH11J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t38 = -t28 + t30
      t50 = bbarbbarH11J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t53 = lh * t20
      t54 = x2 * x3
      t55 = t9 * t12
      t56 = t55 * t4
      t59 = log(-0.4D1 * t54 * t56)
      t65 = t59 ** 2
      t71 = t38 * t20
      t72 = t71 * t50
      t74 = 0.1D1 / x2
      t77 = x1 ** 2
      t78 = t54 * t77
      t81 = log(-0.4D1 * t78 * t56)
      t90 = 0.1D1 / x1
      t93 = x3 * t77
      t96 = log(-0.4D1 * t93 * t56)
      t102 = t96 ** 2
      t111 = -(0.180D3 * lh + 0.90D2 * t16) * t20 * t22 / 0.5760D4 - (-0
     #.180D3 * t16 * lh - t28 + t30 - 0.45D2 * t31) * t20 * t35 / 0.5760
     #D4 - (-t16 * t38 + 0.90D2 * t31 * lh - 0.60D2 * lh * t29 + 0.28849
     #36567583026D3 + 0.120D3 * t27 * lh + 0.15D2 * t31 * t16) * t20 * t
     #50 / 0.5760D4 + (0.180D3 * t53 * (-t35 + t59 * t50) - 0.90D2 * t20
     # * (-t22 + t59 * t35 - t65 * t50 / 0.2D1) - t72) * t74 / 0.5760D4 
     #+ (-0.90D2 * t20 * (-t35 + t81 * t50) - 0.180D3 * t53 * t50) * t74
     # * t90 / 0.2880D4 + (0.180D3 * t53 * (-t35 + t96 * t50) - 0.90D2 *
     # t20 * (-t22 + t96 * t35 - t102 * t50 / 0.2D1) - t72) * t90 / 0.28
     #80D4
      t112 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t
     #111)
      t114 = -0.1D1 + x1
      t115 = x1 * x3
      t116 = t115 * z
      t117 = 0.2D1 * t54
      t118 = t54 * x1
      t119 = x1 * z
      t120 = t54 * t119
      t121 = cos(t7)
      t122 = -0.1D1 + x2
      t124 = 0.1D1 - x1 + t119
      t128 = Sqrt(x3 * t122 * t124 * x2 * t4)
      t130 = 0.2D1 * t121 * t128
      t133 = 0.1D1 / t124
      t136 = t2 * t115
      t137 = x2 * x1
      t138 = t137 * z
      t139 = 0.1D1 - x1 + t119 - x2 + t137 - t138 - x3 + t115 - t116 + t
     #117 - t118 + t120 + t130
      t143 = t4 * s
      t145 = t143 * t1 * x1
      t146 = t1 ** 2
      t152 = x2 * z
      t154 = 0.1D1 / (-0.1D1 + x2 - t137 - t152 + t138 - t119 + x1)
      t155 = t124 * t154
      t156 = -t114
      t157 = bbarbbarH11J2(s, XB1, XB2, z, lh, wd, t156, x2, x3, x4)
      t160 = t54 * t77 * t9
      t161 = t12 * t133
      t162 = t114 ** 2
      t168 = log(0.4D1 * t160 * t161 * t162 * t122 * t4)
      t170 = bbarbbarH11J1(s, XB1, XB2, z, lh, wd, t156, x2, x3, x4)
      t179 = -0.90D2 * t20 * (t155 * t157 - t168 * t124 * t154 * t170) +
     # 0.180D3 * t53 * t155 * t170
      t183 = FJET(XB1, XB2, s, t2 * t114 * (-x3 + t115 - t116 + t117 - t
     #118 + t120 - x2 + t130) * t133, t136, -t2 * t114 * t139 * t133, -t
     #145, -s * t146 * x2 * t114 * x1 * t133, t179 * t74 * t90 / 0.2880D
     #4)
      t191 = Sqrt(x2 * t122 * x3 * t4)
      t193 = 0.2D1 * t121 * t191
      t199 = 0.1D1 / (0.1D1 + t152 - x2)
      t200 = bbarbbarH11J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t201 = t199 * t200
      t207 = log(0.4D1 * t54 * t9 * t12 * t122 * t4)
      t208 = t207 * t199
      t209 = bbarbbarH11J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t214 = bbarbbarH11J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t217 = t207 ** 2
      t224 = t199 * t209
      t233 = log(0.4D1 * t78 * t55 * t122 * t4)
      t245 = (-0.180D3 * t53 * (-t201 + t208 * t209) + 0.90D2 * t20 * (-
     #t199 * t214 + t208 * t200 - t217 * t199 * t209 / 0.2D1) + t71 * t2
     #24) * t74 / 0.5760D4 + (-0.90D2 * t20 * (t201 - t233 * t199 * t209
     #) + 0.180D3 * t53 * t224) * t74 * t90 / 0.2880D4
      t246 = FJET(XB1, XB2, s, -t2 * (-x3 + t117 - x2 + t193), 0.0D0, t2
     # * (0.1D1 - x2 - x3 + t117 + t193), 0.0D0, 0.0D0, t245)
      t252 = bbarbbarH11J2(s, XB1, XB2, z, lh, wd, t156, 0.0D0, x3, x4)
      t254 = t161 * t162 * t4
      t257 = log(-0.4D1 * t160 * t254)
      t258 = bbarbbarH11J1(s, XB1, XB2, z, lh, wd, t156, 0.0D0, x3, x4)
      t271 = log(-0.4D1 * t93 * t9 * t254)
      t276 = bbarbbarH11J3(s, XB1, XB2, z, lh, wd, t156, 0.0D0, x3, x4)
      t278 = t271 ** 2
      t288 = (0.90D2 * t20 * (-t252 + t257 * t258) + 0.180D3 * t53 * t25
     #8) * t74 * t90 / 0.2880D4 + (-0.180D3 * t53 * (-t252 + t271 * t258
     #) + 0.90D2 * t20 * (-t276 + t271 * t252 - t278 * t258 / 0.2D1) + t
     #71 * t258) * t90 / 0.2880D4
      t289 = FJET(XB1, XB2, s, -t2 * t114 * x3, t136, t143 * t1 * t114, 
     #-t145, 0.0D0, t288)
      bbarbbarH1n1e1 = t112 * t111 + t183 * t179 * t74 * t90 / 0.2880D4 
     #+ t246 * t245 + t289 * t288

      end function



      doubleprecision function bbarbbarH1n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH11J1
      doubleprecision bbarbbarH11J2
      doubleprecision bbarbbarH11J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = bbarbbarH11J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t9 = x2 * x3
      t10 = x4 * 0.3141592653589793D1
      t11 = Sin(t10)
      t12 = t11 ** 2
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t16 = t12 * t14 * t4
      t19 = log(-0.4D1 * t9 * t16)
      t20 = bbarbbarH11J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t25 = lh * t7
      t27 = 0.180D3 * t25 * t20
      t29 = 0.1D1 / x2
      t33 = 0.1D1 / x1
      t34 = t29 * t33
      t37 = x1 ** 2
      t38 = x3 * t37
      t41 = log(-0.4D1 * t16 * t38)
      t49 = bbarbbarH11J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t57 = log(-0.4D1 * x3 * t12 * t14 * t4)
      t65 = lh ** 2
      t67 = 0.3141592653589793D1 ** 2
      t69 = t57 ** 2
      t75 = (-0.90D2 * t7 * (-t8 + t19 * t20) - t27) * t29 / 0.5760D4 + 
     #t7 * t20 * t34 / 0.32D2 + (-0.90D2 * t7 * (-t8 + t41 * t20) - t27)
     # * t33 / 0.2880D4 + t7 * t49 / 0.64D2 - (0.180D3 * lh + 0.90D2 * t
     #57) * t7 * t8 / 0.5760D4 - (-0.180D3 * t57 * lh - 0.180D3 * t65 + 
     #0.30D2 * t67 - 0.45D2 * t69) * t7 * t20 / 0.5760D4
      t76 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t7
     #5)
      t78 = -0.1D1 + x1
      t79 = x1 * x3
      t80 = t79 * z
      t81 = 0.2D1 * t9
      t82 = t9 * x1
      t83 = x1 * z
      t84 = t9 * t83
      t85 = cos(t10)
      t86 = -0.1D1 + x2
      t88 = 0.1D1 - x1 + t83
      t92 = Sqrt(x3 * t86 * t88 * x2 * t4)
      t94 = 0.2D1 * t85 * t92
      t97 = 0.1D1 / t88
      t100 = t2 * t79
      t101 = x2 * x1
      t102 = t101 * z
      t103 = 0.1D1 - x1 + t83 - x2 + t101 - t102 - x3 + t79 - t80 + t81 
     #- t82 + t84 + t94
      t107 = t4 * s
      t109 = t107 * t1 * x1
      t110 = t1 ** 2
      t117 = x2 * z
      t119 = 0.1D1 / (-0.1D1 + x2 - t101 - t117 + t102 - t83 + x1)
      t121 = -t78
      t122 = bbarbbarH11J1(s, XB1, XB2, z, lh, wd, t121, x2, x3, x4)
      t127 = FJET(XB1, XB2, s, t2 * t78 * (-x3 + t79 - t80 + t81 - t82 +
     # t84 - x2 + t94) * t97, t100, -t2 * t78 * t103 * t97, -t109, -s * 
     #t110 * x2 * t78 * x1 * t97, -t7 * t88 * t119 * t122 * t29 * t33 / 
     #0.32D2)
      t137 = Sqrt(x2 * t86 * x3 * t4)
      t139 = 0.2D1 * t85 * t137
      t145 = 0.1D1 / (0.1D1 + t117 - x2)
      t146 = bbarbbarH11J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t153 = log(0.4D1 * t9 * t12 * t14 * t86 * t4)
      t155 = bbarbbarH11J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t171 = (0.90D2 * t7 * (-t145 * t146 + t153 * t145 * t155) + 0.180D
     #3 * t25 * t145 * t155) * t29 / 0.5760D4 - t7 * t145 * t155 * t29 *
     # t33 / 0.32D2
      t172 = FJET(XB1, XB2, s, -t2 * (-x3 + t81 - x2 + t139), 0.0D0, t2 
     #* (0.1D1 - x2 - x3 + t81 + t139), 0.0D0, 0.0D0, t171)
      t178 = bbarbbarH11J1(s, XB1, XB2, z, lh, wd, t121, 0.0D0, x3, x4)
      t182 = bbarbbarH11J2(s, XB1, XB2, z, lh, wd, t121, 0.0D0, x3, x4)
      t185 = t78 ** 2
      t190 = log(-0.4D1 * t38 * t12 * t14 * t97 * t185 * t4)
      t200 = -t7 * t178 * t34 / 0.32D2 + (0.90D2 * t7 * (-t182 + t190 * 
     #t178) + 0.180D3 * t25 * t178) * t33 / 0.2880D4
      t201 = FJET(XB1, XB2, s, -t2 * t78 * x3, t100, t107 * t1 * t78, -t
     #109, 0.0D0, t200)
      bbarbbarH1n1e0 = t76 * t75 - t127 * t7 * t88 * t119 * t122 * t34 /
     # 0.32D2 + t172 * t171 + t201 * t200

      end function



      doubleprecision function bbarbbarH1n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH11J1
      doubleprecision bbarbbarH11J2
      doubleprecision bbarbbarH11J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = bbarbbarH11J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t12 = x4 * 0.3141592653589793D1
      t13 = Sin(t12)
      t14 = t13 ** 2
      t16 = z ** 2
      t21 = log(-0.4D1 * x3 * t14 / t16 * t4)
      t25 = bbarbbarH11J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t28 = t7 * t25
      t29 = 0.1D1 / x2
      t32 = 0.1D1 / x1
      t35 = t7 * t8 / 0.64D2 - (0.180D3 * lh + 0.90D2 * t21) * t7 * t25 
     #/ 0.5760D4 + t28 * t29 / 0.64D2 + t28 * t32 / 0.32D2
      t36 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t3
     #5)
      t39 = 0.2D1 * x2 * x3
      t40 = cos(t12)
      t45 = Sqrt(x2 * (-0.1D1 + x2) * x3 * t4)
      t47 = 0.2D1 * t40 * t45
      t54 = 0.1D1 / (0.1D1 + x2 * z - x2)
      t56 = bbarbbarH11J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t60 = FJET(XB1, XB2, s, -t2 * (-x3 + t39 - x2 + t47), 0.0D0, t2 * 
     #(0.1D1 - x2 - x3 + t39 + t47), 0.0D0, 0.0D0, -t7 * t54 * t56 * t29
     # / 0.64D2)
      t66 = -0.1D1 + x1
      t71 = t4 * s
      t77 = bbarbbarH11J1(s, XB1, XB2, z, lh, wd, -t66, 0.0D0, x3, x4)
      t81 = FJET(XB1, XB2, s, -t2 * t66 * x3, t2 * x1 * x3, t71 * t1 * t
     #66, -t71 * t1 * x1, 0.0D0, -t7 * t77 * t32 / 0.32D2)
      bbarbbarH1n1em1 = t36 * t35 - t60 * t7 * t54 * t56 * t29 / 0.64D2 
     #- t81 * t7 * t77 * t32 / 0.32D2

      end function



      doubleprecision function bbarbbarH1n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH11J1
      doubleprecision bbarbbarH11J2
      doubleprecision bbarbbarH11J3
      t2 = s * (-0.1D1 + z)
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = bbarbbarH11J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t11 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * (-0.1D1 + x3), 0.0D0
     #, 0.0D0, t7 * t8 / 0.64D2)
      bbarbbarH1n1em2 = t11 * t7 * t8 / 0.64D2

      end function



      doubleprecision function bbarbbarH1n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH11J1
      doubleprecision bbarbbarH11J2
      doubleprecision bbarbbarH11J3
      bbarbbarH1n1em3 = 0.0D0

      end function



      doubleprecision function bbarbbarH1n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH11J1
      doubleprecision bbarbbarH11J2
      doubleprecision bbarbbarH11J3
      bbarbbarH1n1em4 = 0.0D0

      end function


      doubleprecision function bbarbbarH1n2e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH11J1
      doubleprecision bbarbbarH11J2
      doubleprecision bbarbbarH11J3
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
      t26 = s ** 2
      t27 = 0.1D1 / t26
      t29 = bbarbbarH11J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t32 = -t20 + t22
      t44 = bbarbbarH11J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t47 = t27 * lh
      t48 = bbarbbarH11J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t49 = x2 * x3
      t51 = -0.1D1 + x2
      t56 = log(0.4D1 * t49 * t8 * t11 * t51 * t4)
      t57 = bbarbbarH11J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t59 = t12 * t4
      t62 = log(-0.4D1 * t49 * t59)
      t67 = bbarbbarH11J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t69 = t62 ** 2
      t72 = bbarbbarH11J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t74 = t56 ** 2
      t80 = t32 * t27
      t81 = t57 - t44
      t84 = 0.1D1 / x2
      t93 = x1 ** 2
      t94 = t49 * t93
      t99 = log(0.4D1 * t94 * t12 * t51 * t4)
      t103 = log(-0.4D1 * t94 * t59)
      t113 = 0.1D1 / x1
      t116 = x3 * t93
      t119 = log(-0.4D1 * t116 * t59)
      t125 = t119 ** 2
      t135 = -(-0.180D3 * t16 * lh - t20 + t22 - 0.45D2 * t23) * t27 * t
     #29 / 0.5760D4 - (-t16 * t32 + 0.90D2 * t23 * lh - 0.60D2 * lh * t2
     #1 + 0.2884936567583026D3 + 0.120D3 * t19 * lh + 0.15D2 * t23 * t16
     #) * t27 * t44 / 0.5760D4 + (0.180D3 * t47 * (t48 - t56 * t57 - t29
     # + t62 * t44) - 0.90D2 * t27 * (-t67 + t62 * t29 - t69 * t44 / 0.2
     #D1 + t72 - t56 * t48 + t74 * t57 / 0.2D1) + t80 * t81) * t84 / 0.5
     #760D4 - (0.180D3 * lh + 0.90D2 * t16) * t27 * t67 / 0.5760D4 - (-0
     #.90D2 * t27 * (-t48 + t99 * t57 + t29 - t103 * t44) - 0.180D3 * t4
     #7 * t81) * t84 * t113 / 0.2880D4 + (0.180D3 * t47 * (-t29 + t119 *
     # t44) - 0.90D2 * t27 * (-t67 + t119 * t29 - t125 * t44 / 0.2D1) - 
     #t80 * t44) * t113 / 0.2880D4
      t136 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t
     #135)
      t138 = x1 * x3
      t140 = -0.1D1 + x1
      t142 = t2 * t140 * x3
      t143 = t4 * s
      t147 = t143 * t1 * t140
      t148 = bbarbbarH11J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t150 = t49 * t93 * t11
      t151 = 0.1D1 / t6
      t152 = t140 ** 2
      t154 = x1 * z
      t155 = -z - x1 + t154
      t156 = 0.1D1 / t155
      t158 = t151 * t152 * t156 * t4
      t161 = log(0.4D1 * t150 * t158)
      t162 = bbarbbarH11J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t175 = log(0.4D1 * t116 * t11 * t158)
      t180 = bbarbbarH11J3(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t182 = t175 ** 2
      t192 = -(-0.90D2 * t27 * (-t148 + t161 * t162) - 0.180D3 * t47 * t
     #162) * t84 * t113 / 0.2880D4 + (0.180D3 * t47 * (t148 - t175 * t16
     #2) - 0.90D2 * t27 * (t180 - t175 * t148 + t182 * t162 / 0.2D1) + t
     #80 * t162) * t113 / 0.2880D4
      t193 = FJET(XB1, XB2, s, t2 * t138, -t142, -t143 * t1 * x1, t147, 
     #0.0D0, t192)
      t195 = x3 * z
      t196 = t138 * z
      t197 = t49 * z
      t198 = t49 * x1
      t199 = t49 * t154
      t200 = cos(t9)
      t205 = Sqrt(-x3 * t51 * t155 * x2 * t4)
      t207 = 0.2D1 * t200 * t205
      t213 = x2 * x1
      t214 = t213 * z
      t215 = z + x1 - t154 - x2 * z - t213 + t214 - t195 - t138 + t196 +
     # t197 + t198 - t199 + t49 + t207
      t219 = t1 ** 2
      t226 = 0.1D1 / (z + x1 - t154 - t213 + t214)
      t227 = t155 * t226
      t228 = bbarbbarH11J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t236 = log(-0.4D1 * t150 * t151 * t156 * t152 * t51 * t4)
      t238 = bbarbbarH11J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t247 = 0.90D2 * t27 * (t227 * t228 - t236 * t155 * t226 * t238) - 
     #0.180D3 * t47 * t227 * t238
      t251 = FJET(XB1, XB2, s, t2 * x1 * (-t195 - t138 + t196 + t197 + t
     #198 - t199 - x2 + t49 + t207) * t156, -t142, -t2 * x1 * t215 * t15
     #6, t147, s * t219 * x2 * x1 * t140 * t156, -t247 * t84 * t113 / 0.
     #2880D4)
      bbarbbarH1n2e1 = t136 * t135 + t193 * t192 - t251 * t247 * t84 * t
     #113 / 0.2880D4

      end function



      doubleprecision function bbarbbarH1n2e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH11J1
      doubleprecision bbarbbarH11J2
      doubleprecision bbarbbarH11J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = bbarbbarH11J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t9 = x2 * x3
      t10 = z ** 2
      t12 = 0.1D1 / t10 / z
      t14 = x4 * 0.3141592653589793D1
      t15 = Sin(t14)
      t16 = t15 ** 2
      t17 = -0.1D1 + x2
      t22 = log(0.4D1 * t9 * t12 * t16 * t17 * t4)
      t23 = bbarbbarH11J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t25 = bbarbbarH11J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t26 = t12 * t16
      t27 = t26 * t4
      t30 = log(-0.4D1 * t9 * t27)
      t31 = bbarbbarH11J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t36 = lh * t7
      t37 = t23 - t31
      t41 = 0.1D1 / x2
      t46 = 0.1D1 / x1
      t47 = t41 * t46
      t50 = x1 ** 2
      t51 = x3 * t50
      t54 = log(-0.4D1 * t51 * t27)
      t64 = bbarbbarH11J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t71 = log(-0.4D1 * t26 * x3 * t4)
      t79 = lh ** 2
      t81 = 0.3141592653589793D1 ** 2
      t83 = t71 ** 2
      t89 = (-0.90D2 * t7 * (t8 - t22 * t23 - t25 + t30 * t31) + 0.180D3
     # * t36 * t37) * t41 / 0.5760D4 - t7 * t37 * t47 / 0.32D2 + (-0.90D
     #2 * t7 * (-t25 + t54 * t31) - 0.180D3 * t36 * t31) * t46 / 0.2880D
     #4 + t7 * t64 / 0.64D2 - (0.180D3 * lh + 0.90D2 * t71) * t7 * t25 /
     # 0.5760D4 - (-0.180D3 * t71 * lh - 0.180D3 * t79 + 0.30D2 * t81 - 
     #0.45D2 * t83) * t7 * t31 / 0.5760D4
      t90 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t8
     #9)
      t92 = x1 * x3
      t94 = -0.1D1 + x1
      t96 = t2 * t94 * x3
      t97 = t4 * s
      t101 = t97 * t1 * t94
      t102 = bbarbbarH11J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t106 = bbarbbarH11J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t109 = x1 * z
      t110 = -z - x1 + t109
      t111 = 0.1D1 / t110
      t113 = t94 ** 2
      t118 = log(0.4D1 * t51 * t16 / t10 * t111 * t113 * t4)
      t128 = -t7 * t102 * t47 / 0.32D2 + (-0.90D2 * t7 * (t106 - t118 * 
     #t102) + 0.180D3 * t36 * t102) * t46 / 0.2880D4
      t129 = FJET(XB1, XB2, s, t2 * t92, -t96, -t97 * t1 * x1, t101, 0.0
     #D0, t128)
      t131 = x3 * z
      t132 = t92 * z
      t133 = t9 * z
      t134 = t9 * x1
      t135 = t9 * t109
      t136 = cos(t14)
      t141 = Sqrt(-x3 * t17 * t110 * x2 * t4)
      t143 = 0.2D1 * t136 * t141
      t149 = x2 * x1
      t150 = t149 * z
      t151 = z + x1 - t109 - x2 * z - t149 + t150 - t131 - t92 + t132 + 
     #t133 + t134 - t135 + t9 + t143
      t155 = t1 ** 2
      t163 = 0.1D1 / (z + x1 - t109 - t149 + t150)
      t165 = bbarbbarH11J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t170 = FJET(XB1, XB2, s, t2 * x1 * (-t131 - t92 + t132 + t133 + t1
     #34 - t135 - x2 + t9 + t143) * t111, -t96, -t2 * x1 * t151 * t111, 
     #t101, s * t155 * x2 * x1 * t94 * t111, -t7 * t110 * t163 * t165 * 
     #t41 * t46 / 0.32D2)
      bbarbbarH1n2e0 = t90 * t89 + t129 * t128 - t170 * t7 * t110 * t163
     # * t165 * t47 / 0.32D2

      end function



      doubleprecision function bbarbbarH1n2em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH11J1
      doubleprecision bbarbbarH11J2
      doubleprecision bbarbbarH11J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = bbarbbarH11J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t12 = z ** 2
      t16 = Sin(x4 * 0.3141592653589793D1)
      t17 = t16 ** 2
      t22 = log(-0.4D1 / t12 / z * t17 * x3 * t4)
      t26 = bbarbbarH11J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t29 = bbarbbarH11J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t36 = 0.1D1 / x1
      t39 = t7 * t8 / 0.64D2 - (0.180D3 * lh + 0.90D2 * t22) * t7 * t26 
     #/ 0.5760D4 - t7 * (t29 - t26) / x2 / 0.64D2 + t7 * t26 * t36 / 0.3
     #2D2
      t40 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t3
     #9)
      t44 = -0.1D1 + x1
      t47 = t4 * s
      t52 = bbarbbarH11J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t56 = FJET(XB1, XB2, s, t2 * x1 * x3, -t2 * t44 * x3, -t47 * t1 * 
     #x1, t47 * t1 * t44, 0.0D0, -t7 * t52 * t36 / 0.32D2)
      bbarbbarH1n2em1 = t40 * t39 - t56 * t7 * t52 * t36 / 0.32D2

      end function



      doubleprecision function bbarbbarH1n2em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH11J1
      doubleprecision bbarbbarH11J2
      doubleprecision bbarbbarH11J3
      t2 = s * (-0.1D1 + z)
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = bbarbbarH11J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t11 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * (-0.1D1 + x3)
     #, 0.0D0, t7 * t8 / 0.64D2)
      bbarbbarH1n2em2 = t11 * t7 * t8 / 0.64D2

      end function



      doubleprecision function bbarbbarH1n2em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH11J1
      doubleprecision bbarbbarH11J2
      doubleprecision bbarbbarH11J3
      bbarbbarH1n2em3 = 0.0D0

      end function



      doubleprecision function bbarbbarH1n2em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarbbarH11J1
      doubleprecision bbarbbarH11J2
      doubleprecision bbarbbarH11J3
      bbarbbarH1n2em4 = 0.0D0

      end function
  
 

      doubleprecision function bbarbbarH11J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = x1 * t1
      t4 = z + t3
      t5 = 0.1D1 / t4
      t6 = x1 * t5
      t7 = 0.1D1 - x3
      t8 = 0.1D1 - x2
      t13 = cos(x4 * 0.3141592653589793D1)
      t14 = x3 * t8
      t18 = Sqrt(t14 * t4 * x2 * t7)
      t20 = 0.2D1 * t13 * t18
      t21 = t7 * t8 * t4 + x2 * x3 + t20
      t22 = t6 * t21
      t24 = 0.1D1 - x1
      t25 = t24 * t7
      t27 = s - t2 * t22 - t2 * t25
      t28 = t27 ** 2
      t29 = s ** 2
      t30 = t29 ** 2
      t31 = t28 * t30
      t32 = t1 ** 2
      t33 = t31 * t32
      t34 = x1 ** 2
      t36 = x2 * t7
      t37 = t14 * t4 + t36 - t20
      t40 = t24 * x3
      t42 = s - t2 * t6 * t37 - t2 * t40
      t43 = t42 ** 2
      t44 = t34 * t43
      t45 = t5 * t21
      t49 = t30 * s
      t50 = t28 * t49
      t51 = t50 * t32
      t52 = t34 * t42
      t56 = t32 * t1
      t57 = t50 * t56
      t58 = t34 * x1
      t59 = t58 * t42
      t60 = t4 ** 2
      t61 = 0.1D1 / t60
      t62 = t21 ** 2
      t63 = t61 * t62
      t66 = t28 * t43
      t67 = t66 * t30
      t68 = t5 * t37
      t76 = x1 * t43
      t80 = t27 * t49
      t81 = t80 * t56
      t82 = t24 ** 2
      t83 = t7 ** 2
      t87 = t32 ** 2
      t88 = t87 * t1
      t89 = t82 ** 2
      t93 = z + x1 * t8 * t1
      t94 = t88 * t89 * t93
      t96 = t61 * t43
      t97 = x2 * x1
      t98 = x3 ** 2
      t102 = t88 * t58
      t105 = x2 * t82
      t106 = t61 * t7
      t111 = t87 * t32
      t112 = t82 * t24
      t113 = t111 * t112
      t116 = 0.1D1 / t60 / t4
      t117 = t93 * t116
      t118 = x2 ** 2
      t120 = t117 * t59 * t118
      t123 = t56 * t82
      t124 = t80 * t123
      t125 = t93 * t61
      t126 = t43 * x2
      t128 = t125 * t126 * x1
      t131 = t87 * t82
      t132 = t131 * t93
      t133 = t50 * t132
      t134 = t116 * t42
      t135 = x2 * t34
      t141 = t87 * t112 * t93
      t142 = t50 * t141
      t143 = t61 * t42
      t144 = t97 * x3
      t147 = t87 * t58
      t148 = t80 * t147
      t149 = t24 * t61
      t150 = t149 * t21
      t151 = t126 * t150
      t154 = t87 * t34
      t155 = t80 * t154
      t156 = t82 * t5
      t157 = t156 * x3
      t160 = t31 * t141
      t161 = t97 * t7
      t162 = t96 * t161
      t165 = t30 * t29
      t166 = t27 * t165
      t173 = -0.3D1 * t33 * t44 * t45 - 0.4D1 * t51 * t52 * t45 + t57 * 
     #t59 * t63 - 0.2D1 * t67 * t3 * t68 - 0.6D1 * t67 * z * t1 * t25 - 
     #0.6D1 * t33 * t76 * t25 + t81 * t76 * t82 * t83 + t80 * t94 * t96 
     #* t97 * t98 - 0.2D1 * t80 * t102 * t43 * t105 * t106 * t37 + 0.2D1
     # * t50 * t113 * t120 - 0.4D1 * t124 * t128 + 0.7D1 * t133 * t134 *
     # t135 * t21 + t142 * t143 * t144 + 0.7D1 * t148 * t151 + t155 * t1
     #26 * t157 - 0.3D1 * t160 * t162 + 0.4D1 * t166 * t113 * t120 + 0.4
     #D1 * t142 * t143 * t161
      t174 = t32 * t82
      t175 = t80 * t174
      t176 = t93 * t5
      t178 = t176 * t43 * x3
      t181 = t56 * t112
      t186 = t28 * t165
      t187 = t87 * t56
      t188 = t187 * t89
      t190 = t58 * t118
      t195 = t58 * t43
      t196 = t37 ** 2
      t197 = t61 * t196
      t201 = t66 * t30 * t32
      t205 = t34 ** 2
      t206 = t111 * t205
      t208 = t42 * t118
      t214 = t50 * t147
      t215 = t42 * x2
      t216 = t149 * t37
      t221 = t66 * t30 * t56
      t225 = t80 * t181
      t229 = t24 * t5
      t236 = t60 ** 2
      t237 = 0.1D1 / t236
      t238 = t237 * t205
      t245 = t166 * t111 * t82 * t93
      t250 = t88 * t112
      t252 = t43 * t118
      t254 = t117 * t252 * t34
      t257 = t156 * t7
      t263 = t125 * t44 * x2
      t266 = t1 * t24
      t273 = t88 * t82 * t93
      t275 = t116 * t58
      t276 = x2 * z
      t277 = t276 * t37
      t284 = -0.4D1 * t175 * t178 + t50 * t181 * t176 * t42 * t83 + 0.2D
     #1 * t186 * t188 * t117 * t190 * x3 + t81 * t195 * t197 + 0.12D2 * 
     #t201 * t25 * t22 - 0.4D1 * t50 * t206 * t208 * t82 * t116 * t21 + 
     #0.2D1 * t214 * t215 * t216 + 0.5D1 * t221 * t135 * t216 + t225 * t
     #176 * t43 * t83 + 0.4D1 * t201 * t97 * t229 - 0.2D1 * t166 * t187 
     #* t112 * t93 * t238 * t208 * t37 + 0.6D1 * t245 * t238 * t215 * t1
     #96 - 0.4D1 * t80 * t250 * t254 - 0.2D1 * t155 * t126 * t257 - 0.8D
     #1 * t80 * t131 * t263 + 0.6D1 * t31 * t266 * t176 * t43 * z + 0.6D
     #1 * t186 * t273 * t275 * t277 - 0.8D1 * t221 * t135 * t150
      t286 = t80 * t141
      t289 = t96 * t144
      t297 = t165 * t111
      t298 = t238 * t21
      t300 = t82 * t93
      t301 = t126 * t37
      t305 = t275 * t21
      t308 = t126 * x3
      t313 = t111 * t89 * t93
      t315 = t61 * t34
      t325 = t56 * t24
      t326 = t50 * t325
      t331 = t126 * t216
      t334 = t165 * z
      t336 = t334 * t206 * t237
      t338 = t93 * t43
      t345 = t126 * z
      t354 = t50 * t123 * t93
      t360 = t56 * t34
      t361 = t80 * t360
      t368 = t50 * t123
      t373 = t250 * t93
      t374 = t50 * t373
      t376 = t315 * t215 * x3
      t380 = t24 * t93
      t382 = t380 * t5 * t43
      t385 = x1 * t42
      t390 = 0.2D1 * t286 * t162 + 0.4D1 * t286 * t289 + 0.2D1 * t80 * t
     #113 * t117 * t195 * t118 - 0.2D1 * t297 * t298 * t300 * t301 - 0.2
     #D1 * t297 * t305 * t112 * t93 * t308 - 0.2D1 * t186 * t313 * t315 
     #* t36 * x3 + 0.4D1 * t50 * t131 * t125 * t52 * x2 + 0.3D1 * t326 *
     # t125 * t52 * t21 + 0.4D1 * t148 * t331 - 0.6D1 * t336 * t62 * t82
     # * t338 * x2 + 0.6D1 * t165 * t88 * t305 * t300 * t345 + 0.2D1 * t
     #50 * t154 * t215 * t257 - 0.2D1 * t354 * t143 * t7 * x1 * t37 - 0.
     #2D1 * t361 * t43 * t24 * t7 * t5 * t37 - 0.12D2 * t368 * t125 * t2
     #15 * x1 + 0.3D1 * t374 * t376 + 0.4D1 * t80 * t1 * t382 + 0.3D1 * 
     #t368 * t176 * t385 * t7
      t391 = t166 * t132
      t392 = z ** 2
      t397 = t31 * t147
      t400 = t43 * t7
      t414 = t31 * t174
      t415 = t176 * t400
      t421 = t50 * t273
      t424 = x2 * t58 * t196
      t428 = t315 * t215 * t7
      t434 = t166 * t273
      t435 = t275 * t42
      t442 = t113 * t93
      t444 = x2 * t21
      t466 = t229 * z
      t470 = 0.6D1 * t391 * t315 * t215 * t392 - 0.3D1 * t397 * t151 + 0
     #.2D1 * t225 * t176 * t400 * x3 + 0.3D1 * t124 * t176 * t76 * x3 + 
     #0.3D1 * t80 * t325 * t125 * t44 * t37 - 0.2D1 * t414 * t415 + t225
     # * t176 * t43 * t98 + t421 * t237 * t42 * t424 - 0.4D1 * t374 * t4
     #28 + 0.6D1 * t66 * t30 * t392 - 0.6D1 * t434 * t435 * t277 - 0.13D
     #2 * t221 * t97 * t257 - 0.2D1 * t186 * t442 * t275 * t444 * x3 + t
     #326 * t117 * t52 * t196 + 0.6D1 * t50 * t250 * t117 * t208 * t34 +
     # 0.12D2 * t245 * t238 * t42 * t444 * t37 - 0.6D1 * t354 * t143 * t
     #97 * z + 0.6D1 * t201 * t97 * t466
      t473 = t88 * t205
      t475 = t24 * t116
      t479 = t88 * t34
      t482 = t112 * t5 * t83
      t486 = t126 * t229
      t489 = t80 * t273
      t490 = t237 * t43
      t493 = t32 * t24
      t499 = t176 * t76
      t503 = t275 * t215 * t21
      t511 = t315 * t215 * z
      t523 = t80 * t132
      t524 = t315 * t345
      t537 = t3 * t43
      t545 = t80 * t473 * t126 * t475 * t196 + t80 * t479 * t126 * t482 
     #- 0.4D1 * t31 * t360 * t486 + t489 * t490 * t424 - 0.2D1 * t50 * t
     #493 * t176 * t385 - 0.2D1 * t80 * t493 * t499 - 0.8D1 * t434 * t50
     #3 + t50 * t473 * t215 * t475 * t62 + 0.6D1 * t391 * t511 - 0.3D1 *
     # t397 * t331 + 0.3D1 * t421 * t503 - 0.6D1 * t66 * t30 * z * t3 * 
     #t45 - 0.6D1 * t523 * t524 + 0.4D1 * t50 * t3 * t42 + 0.2D1 * t489 
     #* t275 * t301 - 0.6D1 * t165 * t87 * t300 * t524 - 0.2D1 * t31 * t
     #537 - 0.6D1 * t336 * t196 * t28 * t300 * x2
      t549 = t31 * t1
      t559 = t135 * t37
      t574 = t93 * t237
      t575 = t118 * x2
      t576 = t43 * t575
      t585 = t187 * t205
      t587 = t42 * t575
      t588 = t112 * t116
      t603 = t116 * t43
      t622 = -0.2D1 * t67 * t266 * t7 + 0.6D1 * t549 * t76 * z + t80 * t
     #537 + 0.8D1 * t31 * t131 * t263 - 0.3D1 * t175 * t415 - 0.2D1 * t1
     #33 * t134 * t559 + 0.6D1 * t245 * t238 * t215 * t62 + 0.2D1 * t50 
     #* t56 * t58 * t143 * t21 * t37 + 0.4D1 * t80 * t188 * t574 * t576 
     #* t58 - 0.6D1 * t434 * t435 * t276 * t21 + 0.4D1 * t50 * t585 * t5
     #87 * t588 - 0.4D1 * t50 * t360 * t215 * t229 - 0.3D1 * t50 * t111 
     #* t58 * t208 * t112 * t61 * t7 + 0.2D1 * t523 * t603 * t559 - 0.6D
     #1 * t186 * t131 * t125 * t135 * z - 0.6D1 * t361 * t126 * t466 + 0
     #.2D1 * t31 * t250 * t254 + t50 * t188 * t574 * t587 * t58
      t625 = t82 * t61
      t641 = t275 * t215 * t37
      t657 = t252 * t625
      t668 = t166 * t373
      t683 = t31 * t493
      t688 = -0.4D1 * t50 * t102 * t208 * t625 + t80 * t585 * t576 * t58
     #8 + 0.3D1 * t414 * t178 - 0.2D1 * t374 * t134 * x2 * t34 * t7 * t3
     #7 + 0.6D1 * t421 * t641 - 0.6D1 * t133 * t511 - 0.2D1 * t221 * t97
     # * t157 + t50 * t479 * t215 * t482 + 0.9D1 * t489 * t275 * t126 * 
     #t21 - 0.12D2 * t80 * t102 * t657 + 0.6D1 * t361 * t486 + 0.6D1 * t
     #67 * t32 * t34 * t63 - 0.13D2 * t434 * t641 - 0.2D1 * t668 * t376 
     #+ 0.5D1 * t668 * t428 + t50 * t94 * t143 * t97 * t83 - 0.2D1 * t16
     #6 * t188 * t93 * t275 * t208 * t7 - 0.9D1 * t683 * t125 * t76 * t2
     #1
      t708 = t80 * t373
      t765 = 0.12D2 * t334 * t111 * t298 * t37 * t27 * t82 * t93 * t42 *
     # x2 - 0.3D1 * t160 * t289 + 0.8D1 * t683 * t499 + 0.2D1 * t31 * t1
     #02 * t657 + 0.6D1 * t67 * t174 * t83 + 0.2D1 * t708 * t603 * x2 * 
     #t34 * t37 * x3 - 0.3D1 * t708 * t315 * t308 + 0.2D1 * t165 * t187 
     #* t238 * t21 * t112 * t338 * t118 - 0.4D1 * t80 * t313 * t603 * t1
     #18 * t34 * x3 + 0.8D1 * t31 * t123 * t128 - 0.3D1 * t80 * t442 * t
     #490 * t190 * t37 + 0.4D1 * t214 * t215 * t150 - 0.3D1 * t33 * t76 
     #* t40 + t50 * t1 * t380 * t5 * t42 + 0.2D1 * t50 * t102 * t42 * t1
     #05 * t106 * t21 - 0.2D1 * t549 * t382 + 0.4D1 * t33 * t44 * t68 - 
     #0.3D1 * t51 * t52 * t68 + t57 * t59 * t197
      bbarbbarH11J1 = 0.16D2 / 0.3D1 * wd * (t173 + t284 + t390 + t470 +
     # t545 + t622 + t688 + t765) / t28 / t43 / t29

      end function
  
   
 

      doubleprecision function bbarbbarH11J2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = x1 * t1
      t4 = z + t3
      t5 = 0.1D1 / t4
      t6 = x1 * t5
      t7 = 0.1D1 - x3
      t8 = 0.1D1 - x2
      t13 = cos(x4 * 0.3141592653589793D1)
      t14 = x3 * t8
      t18 = Sqrt(t14 * t4 * x2 * t7)
      t20 = 0.2D1 * t13 * t18
      t21 = t7 * t8 * t4 + x2 * x3 + t20
      t24 = 0.1D1 - x1
      t25 = t24 * t7
      t27 = s - t2 * t6 * t21 - t2 * t25
      t28 = t27 ** 2
      t29 = s ** 2
      t30 = t29 ** 2
      t31 = t30 * s
      t32 = t28 * t31
      t33 = t1 ** 2
      t34 = t33 ** 2
      t35 = t34 * t1
      t36 = t24 ** 2
      t37 = t36 ** 2
      t41 = z + x1 * t8 * t1
      t42 = t35 * t37 * t41
      t44 = t4 ** 2
      t45 = 0.1D1 / t44
      t47 = x2 * t7
      t48 = t14 * t4 + t47 - t20
      t51 = t24 * x3
      t53 = s - t2 * t6 * t48 - t2 * t51
      t54 = t45 * t53
      t55 = x2 * x1
      t56 = t7 ** 2
      t60 = t27 * t31
      t61 = x1 ** 2
      t62 = t61 * t34
      t63 = t60 * t62
      t64 = t53 ** 2
      t65 = t64 * x2
      t66 = t36 * t5
      t67 = t66 * t7
      t68 = t65 * t67
      t71 = t35 * t36
      t72 = t71 * t41
      t73 = t32 * t72
      t75 = 0.1D1 / t44 / t4
      t76 = t61 * x1
      t77 = t75 * t76
      t78 = t53 * x2
      t80 = t77 * t78 * t48
      t83 = t36 * t24
      t85 = t34 * t83 * t41
      t86 = t60 * t85
      t87 = t45 * t64
      t88 = t55 * x3
      t89 = t87 * t88
      t92 = t34 * t36
      t93 = t92 * t41
      t96 = x2 * t61
      t97 = t96 * t48
      t98 = t75 * t64 * t97
      t101 = t30 * t29
      t102 = t27 * t101
      t103 = t34 * t33
      t104 = t103 * t83
      t106 = t41 * t75
      t107 = t76 * t53
      t108 = x2 ** 2
      t110 = t106 * t107 * t108
      t113 = t35 * t83
      t115 = t64 * t108
      t117 = t106 * t115 * t61
      t120 = t28 * t30
      t124 = t34 * t76
      t125 = t60 * t124
      t126 = t24 * t45
      t127 = t126 * t48
      t128 = t65 * t127
      t131 = t113 * t41
      t132 = t102 * t131
      t133 = t45 * t61
      t135 = t133 * t78 * x3
      t139 = t133 * t78 * t7
      t141 = t33 * t36
      t143 = t41 * t5
      t145 = t143 * t64 * t7
      t149 = t36 * t41
      t155 = t33 * t1
      t156 = t155 * t24
      t157 = t32 * t156
      t158 = t41 * t45
      t159 = t61 * t53
      t164 = t103 * t76
      t167 = t83 * t45 * t7
      t171 = -t32 * t42 * t54 * t55 * t56 + 0.2D1 * t63 * t68 + 0.2D1 * 
     #t73 * t80 - 0.4D1 * t86 * t89 - 0.2D1 * t60 * t93 * t98 - 0.4D1 * 
     #t102 * t104 * t110 + 0.4D1 * t60 * t113 * t117 + 0.4D1 * t120 * t1
     #13 * t117 - 0.4D1 * t125 * t128 + 0.2D1 * t132 * t135 + t132 * t13
     #9 - 0.3D1 * t60 * t141 * t145 + 0.6D1 * t101 * t34 * t149 * t133 *
     # t65 * z + 0.4D1 * t157 * t158 * t159 * t48 + 0.2D1 * t60 * t164 *
     # t115 * t167
      t172 = t155 * t83
      t173 = t60 * t172
      t177 = t155 * t36
      t179 = x1 * t53
      t184 = t60 * t177
      t185 = x1 * t64
      t190 = t48 ** 2
      t195 = t102 * t72
      t197 = t44 ** 2
      t198 = 0.1D1 / t197
      t199 = t198 * t53
      t200 = x2 * t76
      t201 = t200 * t190
      t205 = t28 * t64
      t207 = t205 * t30 * t155
      t210 = t120 * t124
      t211 = t126 * t21
      t212 = t65 * t211
      t216 = t205 * t30 * t33
      t217 = t24 * t5
      t218 = t217 * z
      t223 = x3 ** 2
      t227 = t205 * t30
      t233 = t61 * t64
      t239 = t24 * t41
      t241 = t239 * t5 * t64
      t244 = t60 * t33
      t245 = t185 * t25
      t248 = t60 * t155
      t249 = t76 * t64
      t250 = t45 * t190
      t253 = t5 * t48
      t254 = t233 * t253
      t257 = t173 * t143 * t64 * t56 - 0.3D1 * t32 * t177 * t143 * t179 
     #* t7 - 0.3D1 * t184 * t143 * t185 * x3 - 0.3D1 * t157 * t106 * t15
     #9 * t190 + t195 * t80 - 0.3D1 * t73 * t199 * t201 + t207 * t55 * t
     #67 + 0.3D1 * t210 * t212 + 0.6D1 * t216 * t55 * t218 - t60 * t42 *
     # t87 * t55 * t223 + 0.2D1 * t227 * t1 * t24 * t7 - 0.2D1 * t60 * t
     #92 * t158 * t233 * x2 + 0.2D1 * t60 * t1 * t241 + 0.2D1 * t244 * t
     #245 - t248 * t249 * t250 - 0.2D1 * t244 * t254
      t263 = t32 * t155
      t273 = t120 * t33
      t275 = t28 * t101
      t283 = t21 ** 2
      t296 = t32 * t93
      t297 = t75 * t53
      t301 = t32 * t85
      t304 = t104 * t41
      t306 = t108 * t76
      t307 = t306 * t48
      t311 = t101 * t35
      t312 = t83 * t41
      t314 = t65 * x3
      t321 = -0.3D1 * t32 * t33 * t159 * t253 + t263 * t107 * t250 - t32
     # * t1 * t239 * t5 * t53 - 0.2D1 * t120 * t1 * t241 - t273 * t254 +
     # 0.6D1 * t275 * t92 * t158 * t96 * z + t273 * t185 * t51 - t263 * 
     #t107 * t45 * t283 + 0.2D1 * t227 * t3 * t253 - 0.2D1 * t273 * t245
     # - 0.3D1 * t248 * t185 * t36 * t56 - t296 * t297 * t96 * t21 - t30
     #1 * t54 * t88 + 0.2D1 * t32 * t304 * t199 * t307 - 0.2D1 * t311 * 
     #t312 * t133 * t314 - 0.4D1 * t216 * t55 * t217
      t322 = t34 * t155
      t323 = t61 ** 2
      t324 = t322 * t323
      t326 = t108 * x2
      t327 = t53 * t326
      t328 = t83 * t75
      t332 = t155 * t61
      t337 = t35 * t76
      t339 = t53 * t108
      t340 = t36 * t45
      t344 = t35 * t61
      t347 = t83 * t5 * t56
      t355 = t35 * t323
      t357 = t24 * t75
      t362 = t32 * t177 * t41
      t368 = t60 * t332
      t379 = t322 * t37
      t389 = t198 * t323
      t411 = 0.2D1 * t32 * t324 * t327 * t328 + 0.4D1 * t32 * t332 * t78
     # * t217 + 0.4D1 * t32 * t337 * t339 * t340 - 0.3D1 * t60 * t344 * 
     #t65 * t347 - t125 * t212 + 0.2D1 * t296 * t297 * t97 - t32 * t355 
     #* t78 * t357 * t283 + 0.4D1 * t362 * t54 * t7 * x1 * t48 + 0.4D1 *
     # t368 * t64 * t24 * t7 * t5 * t48 - 0.3D1 * t32 * t164 * t339 * t1
     #67 + 0.2D1 * t102 * t379 * t41 * t77 * t339 * t7 + 0.2D1 * t102 * 
     #t322 * t83 * t41 * t389 * t339 * t48 + t207 * t96 * t127 - 0.2D1 *
     # t60 * t103 * t323 * t115 * t36 * t75 * t48 - 0.4D1 * t120 * t62 *
     # t68 + 0.2D1 * t60 * t131 * t133 * t65 * t7
      t414 = t32 * t131
      t421 = t60 * t72
      t435 = t103 * t37 * t41
      t456 = t64 * t326
      t459 = t101 * t103
      t484 = t120 * t85
      t485 = t55 * t7
      t486 = t87 * t485
      t489 = t33 * t24
      t494 = 0.4D1 * t414 * t297 * x2 * t61 * t7 * t48 - t421 * t77 * t6
     #5 * t21 - t32 * t172 * t143 * t53 * t56 - 0.2D1 * t275 * t379 * t1
     #06 * t306 * x3 + 0.2D1 * t275 * t435 * t133 * t47 * x3 + 0.2D1 * t
     #60 * t104 * t106 * t249 * t108 - 0.3D1 * t60 * t156 * t158 * t233 
     #* t48 + 0.2D1 * t120 * t332 * t65 * t217 - t60 * t324 * t456 * t32
     #8 + 0.2D1 * t459 * t77 * t21 * t312 * t314 - 0.2D1 * t101 * t322 *
     # t389 * t21 * t83 * t41 * t64 * t108 - 0.6D1 * t368 * t65 * t218 +
     # 0.4D1 * t60 * t337 * t64 * x2 * t36 * t45 * t7 * t48 + 0.3D1 * t4
     #84 * t486 - 0.4D1 * t60 * t489 * t143 * t185
      t495 = t65 * t48
      t496 = t77 * t495
      t503 = t3 * t64
      t517 = t158 * t65 * x1
      t521 = t41 * t198
      t533 = t198 * t64
      t549 = t421 * t496 - t173 * t143 * t64 * t223 + 0.3D1 * t210 * t12
     #8 - t60 * t503 + 0.2D1 * t32 * t3 * t53 - 0.2D1 * t120 * t503 - 0.
     #4D1 * t301 * t54 * t485 + 0.2D1 * t207 * t96 * t211 + 0.4D1 * t184
     # * t517 - t32 * t379 * t521 * t327 * t76 + 0.2D1 * t459 * t312 * t
     #77 * t115 + 0.4D1 * t184 * t143 * t185 * t7 + t421 * t533 * t201 -
     # 0.2D1 * t32 * t62 * t78 * t67 - t60 * t355 * t65 * t357 * t190 - 
     #0.2D1 * t32 * t92 * t158 * t159 * x2
      t554 = t120 * t489
      t559 = t32 * t489
      t574 = t66 * x3
      t595 = t32 * t124
      t615 = 0.2D1 * t32 * t104 * t110 - 0.2D1 * t554 * t158 * t185 * t4
     #8 - 0.4D1 * t559 * t143 * t179 - 0.2D1 * t32 * t435 * t297 * t108 
     #* t61 * t7 - t120 * t141 * t145 + t554 * t158 * t185 * t21 + 0.2D1
     # * t207 * t55 * t574 + t32 * t344 * t78 * t347 + 0.6D1 * t102 * t9
     #3 * t133 * t78 * z + 0.2D1 * t459 * t389 * t21 * t149 * t495 - 0.3
     #D1 * t60 * t304 * t533 * t307 - 0.4D1 * t595 * t78 * t211 - 0.4D1 
     #* t120 * t93 * t98 - 0.2D1 * t32 * t141 * t143 * t53 * t7 + 0.2D1 
     #* t195 * t77 * t78 * t21 - 0.3D1 * t157 * t158 * t159 * t21
      t670 = -t63 * t65 * t574 - 0.6D1 * t362 * t54 * t55 * z + 0.2D1 * 
     #t275 * t304 * t77 * x2 * t21 * x3 + 0.2D1 * t559 * t158 * t179 * t
     #48 - 0.2D1 * t595 * t78 * t127 + 0.2D1 * t60 * t379 * t521 * t456 
     #* t76 - 0.2D1 * t311 * t149 * t496 - 0.2D1 * t275 * t113 * t158 * 
     #t96 * t7 - t414 * t135 + t414 * t139 + 0.2D1 * t120 * t177 * t517 
     #+ 0.4D1 * t120 * t337 * t115 * t340 - 0.2D1 * t275 * t71 * t106 * 
     #t200 * t21 + 0.2D1 * t275 * t104 * t106 * t306 + 0.3D1 * t484 * t8
     #9 - 0.2D1 * t86 * t486
      bbarbbarH11J2 = 0.16D2 / 0.3D1 * wd * (t171 + t257 + t321 + t411 +
     # t494 + t549 + t615 + t670) / t28 / t64 / t29

      end function
  
   
 

      doubleprecision function bbarbbarH11J3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + x1 * t1
      t5 = 0.1D1 / t4
      t6 = x1 * t5
      t7 = 0.1D1 - x3
      t8 = 0.1D1 - x2
      t13 = cos(x4 * 0.3141592653589793D1)
      t14 = x3 * t8
      t18 = Sqrt(t14 * t4 * x2 * t7)
      t20 = 0.2D1 * t13 * t18
      t21 = t7 * t8 * t4 + x2 * x3 + t20
      t24 = 0.1D1 - x1
      t25 = t24 * t7
      t27 = s - t2 * t6 * t21 - t2 * t25
      t28 = s ** 2
      t29 = t28 ** 2
      t30 = t29 * s
      t31 = t27 * t30
      t32 = t1 ** 2
      t33 = t32 ** 2
      t34 = t33 * t1
      t35 = t24 ** 2
      t36 = t35 * t24
      t37 = t34 * t36
      t40 = z + x1 * t8 * t1
      t41 = t37 * t40
      t42 = t31 * t41
      t43 = t4 ** 2
      t45 = 0.1D1 / t43 / t4
      t48 = t14 * t4 + x2 * t7 - t20
      t53 = s - t2 * t6 * t48 - t2 * t24 * x3
      t54 = t53 ** 2
      t55 = t45 * t54
      t57 = x1 ** 2
      t63 = t27 ** 2
      t64 = t63 * t54
      t65 = t32 * t1
      t67 = t64 * t29 * t65
      t68 = x2 * x1
      t69 = t35 * t5
      t74 = t63 * t30
      t77 = t53 * x2
      t79 = t7 ** 2
      t83 = t74 * t65
      t84 = t57 * x1
      t85 = t84 * t53
      t86 = 0.1D1 / t43
      t87 = t48 ** 2
      t91 = t33 * t35
      t92 = t91 * t40
      t95 = x2 * t57
      t96 = t95 * t48
      t101 = t31 * t65 * t36
      t102 = t40 * t5
      t107 = t40 * t86
      t108 = t57 * t53
      t113 = t33 * t32
      t114 = t113 * t36
      t117 = t43 ** 2
      t119 = 0.1D1 / t117 * t54
      t120 = x2 ** 2
      t121 = t120 * t84
      t125 = t29 * t28
      t126 = t63 * t125
      t127 = t34 * t35
      t129 = t40 * t45
      t130 = t84 * x2
      t139 = t65 * t35
      t140 = t31 * t139
      t141 = x1 * t54
      t145 = t125 * t34
      t146 = t36 * t40
      t148 = t86 * t57
      t149 = t54 * x2
      t151 = t148 * t149 * x3
      t154 = t65 * t24
      t155 = t74 * t154
      t160 = t27 * t125
      t161 = t127 * t40
      t162 = t160 * t161
      t163 = t45 * t84
      t165 = t163 * t77 * t21
      t169 = t63 * t29
      t170 = t33 * t84
      t171 = t169 * t170
      t172 = t24 * t86
      t173 = t172 * t48
      t174 = t149 * t173
      t176 = -0.2D1 * t42 * t55 * x2 * t57 * t48 * x3 + 0.2D1 * t67 * t6
     #8 * t69 * x3 - t74 * t34 * t57 * t77 * t36 * t5 * t79 - t83 * t85 
     #* t86 * t87 + 0.4D1 * t74 * t92 * t45 * t53 * t96 - t101 * t102 * 
     #t54 * t79 - 0.4D1 * t74 * t91 * t107 * t108 * x2 + t31 * t114 * t4
     #0 * t119 * t121 * t48 + 0.2D1 * t126 * t127 * t129 * t130 * t21 - 
     #0.2D1 * t126 * t114 * t129 * t121 - t140 * t102 * t141 * t7 + 0.2D
     #1 * t145 * t146 * t151 - 0.2D1 * t155 * t107 * t108 * t21 + 0.2D1 
     #* t162 * t165 + t42 * t151 + t171 * t174
      t177 = t74 * t161
      t179 = t33 * t57
      t181 = t69 * t7
      t182 = t149 * t181
      t185 = t172 * t21
      t195 = t57 ** 2
      t198 = t53 * t120
      t218 = t169 * t32
      t219 = t57 * t54
      t220 = t5 * t21
      t223 = t74 * t32
      t227 = t21 ** 2
      t245 = t177 * t165 + 0.4D1 * t31 * t179 * t182 + 0.2D1 * t171 * t1
     #49 * t185 - t42 * t148 * t149 * t7 + t169 * t92 * t55 * t96 + 0.2D
     #1 * t74 * t113 * t195 * t198 * t35 * t45 * t21 + 0.4D1 * t169 * t1
     #39 * t107 * t149 * x1 + 0.2D1 * t126 * t37 * t107 * t95 * t7 - t31
     # * t161 * t119 * t130 * t87 - t218 * t219 * t220 + 0.2D1 * t223 * 
     #t108 * t220 - t83 * t85 * t86 * t227 + t223 * t108 * t5 * t48 + t2
     #18 * t141 * t25 + 0.2D1 * t145 * t35 * t40 * t163 * t149 * t48 + 0
     #.2D1 * t67 * t68 * t181
      t247 = t35 ** 2
      t257 = t163 * t77 * t48
      t260 = t160 * t41
      t271 = t86 * t54
      t272 = x3 ** 2
      t323 = 0.2D1 * t31 * t113 * t247 * t40 * t55 * t120 * t57 * x3 + 0
     #.2D1 * t162 * t257 + 0.2D1 * t260 * t148 * t77 * x3 - 0.4D1 * t31 
     #* t170 * t174 - t31 * t34 * t247 * t40 * t271 * t68 * t272 - 0.4D1
     # * t31 * t91 * t107 * t219 * x2 + 0.2D1 * t67 * t95 * t173 - t74 *
     # t34 * t195 * t77 * t24 * t45 * t227 + t74 * t113 * t84 * t198 * t
     #36 * t86 * t7 - t74 * t139 * t102 * x1 * t53 * t7 - t155 * t107 * 
     #t108 * t48 + t169 * t179 * t182 - 0.2D1 * t140 * t102 * t141 * x3 
     #- t31 * t154 * t107 * t219 * t48 - 0.2D1 * t125 * t113 * t146 * t1
     #63 * t54 * t120 + 0.2D1 * t67 * t95 * t185
      t327 = t24 * t5
      t331 = t32 * t35
      t332 = t31 * t331
      t334 = t102 * t54 * x3
      t347 = t33 * t36 * t40
      t349 = t86 * t53
      t350 = t68 * t7
      t365 = t169 * t347
      t375 = t54 * t7
      t398 = -t177 * t257 + 0.4D1 * t169 * t65 * t57 * t149 * t327 + 0.2
     #D1 * t332 * t334 - 0.2D1 * t74 * t34 * t84 * t53 * x2 * t35 * t86 
     #* t7 * t21 - 0.4D1 * t74 * t347 * t349 * t350 - 0.4D1 * t160 * t11
     #4 * t129 * t85 * t120 - 0.2D1 * t74 * t65 * t84 * t349 * t21 * t48
     # + 0.2D1 * t365 * t271 * t68 * x3 + t365 * t271 * t350 - t101 * t1
     #02 * t54 * t272 + t332 * t102 * t375 - t169 * t331 * t334 + t169 *
     # t32 * t24 * t107 * t141 * t48 + 0.2D1 * t260 * t148 * t77 * t7 - 
     #0.4D1 * t64 * t29 * t32 * t68 * t327 - 0.2D1 * t101 * t102 * t375 
     #* x3
      bbarbbarH11J3 = 0.16D2 / 0.3D1 * wd * (t176 + t245 + t323 + t398) 
     #/ t63 / t54 / t28

      end function
  
 