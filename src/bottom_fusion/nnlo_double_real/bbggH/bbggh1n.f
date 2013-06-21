  
      subroutine bbggh1n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision bbggh11J1  
      doubleprecision bbggh11J2  
      doubleprecision bbggh11J3  
      doubleprecision bbggh1n1e1  
      doubleprecision bbggh1n1e0  
      doubleprecision bbggh1n1em1  
      doubleprecision bbggh1n1em2  
      doubleprecision bbggh1n1em3  
      doubleprecision bbggh1n1em4  
      doubleprecision bbggh1n2e1  
      doubleprecision bbggh1n2e0  
      doubleprecision bbggh1n2em1  
      doubleprecision bbggh1n2em2  
      doubleprecision bbggh1n2em3  
      doubleprecision bbggh1n2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=bbggh1n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbggh1n2e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=bbggh1n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbggh1n2e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=bbggh1n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbggh1n2em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=bbggh1n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbggh1n2em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=bbggh1n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbggh1n2em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=bbggh1n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbggh1n2em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function bbggh1n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh11J1
      doubleprecision bbggh11J2
      doubleprecision bbggh11J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t7 = x4 * 0.3141592653589793D1
      t8 = Sin(t7)
      t9 = t8 ** 2
      t10 = z ** 2
      t11 = 0.1D1 / t10
      t13 = x3 * t4
      t16 = log(-0.4D1 * t9 * t11 * t13)
      t19 = s ** 2
      t20 = 0.1D1 / t19
      t22 = bbggh11J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t27 = lh ** 2
      t28 = 0.180D3 * t27
      t29 = 0.3141592653589793D1 ** 2
      t30 = 0.30D2 * t29
      t31 = t16 ** 2
      t35 = bbggh11J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t38 = t28 - t30
      t50 = bbggh11J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t53 = lh * t20
      t54 = x2 * t9
      t55 = t11 * x3
      t56 = t55 * t4
      t59 = log(-0.4D1 * t54 * t56)
      t65 = t59 ** 2
      t71 = t38 * t20
      t72 = t71 * t50
      t74 = 0.1D1 / x2
      t77 = x1 ** 2
      t79 = x2 * t77 * t9
      t82 = log(-0.4D1 * t79 * t56)
      t90 = 0.1D1 / x1
      t94 = t77 * t9
      t97 = log(-0.4D1 * t94 * t56)
      t103 = t97 ** 2
      t112 = (-0.180D3 * lh - 0.90D2 * t16) * t20 * t22 / 0.5760D4 + (0.
     #180D3 * t16 * lh + t28 - t30 + 0.45D2 * t31) * t20 * t35 / 0.5760D
     #4 + (-t16 * t38 - 0.90D2 * t31 * lh - 0.2884936567583026D3 - 0.120
     #D3 * t27 * lh + 0.60D2 * lh * t29 - 0.15D2 * t31 * t16) * t20 * t5
     #0 / 0.5760D4 - (-0.180D3 * t53 * (-t35 + t59 * t50) + 0.90D2 * t20
     # * (-t22 + t59 * t35 - t65 * t50 / 0.2D1) - t72) * t74 / 0.5760D4 
     #+ (0.90D2 * t20 * (t35 - t82 * t50) - 0.180D3 * t53 * t50) * t90 *
     # t74 / 0.2880D4 + (-0.180D3 * t53 * (t35 - t97 * t50) + 0.90D2 * t
     #20 * (t22 - t97 * t35 + t103 * t50 / 0.2D1) + t72) * t90 / 0.2880D
     #4
      t113 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t
     #112)
      t115 = -0.1D1 + x1
      t116 = x1 * x3
      t117 = t116 * z
      t119 = 0.2D1 * x2 * x3
      t120 = t116 * x2
      t121 = x2 * z
      t122 = t116 * t121
      t123 = cos(t7)
      t124 = -0.1D1 + x2
      t125 = x3 * t124
      t126 = x1 * z
      t127 = 0.1D1 - x1 + t126
      t131 = Sqrt(t125 * t127 * x2 * t4)
      t133 = 0.2D1 * t123 * t131
      t136 = 0.1D1 / t127
      t139 = t2 * t116
      t140 = x1 * x2
      t141 = t140 * z
      t142 = 0.1D1 - x1 + t126 - x2 + t140 - t141 - x3 + t116 - t117 + t
     #119 - t120 + t122 + t133
      t147 = t2 * x1 * t4
      t148 = t1 ** 2
      t155 = 0.1D1 / (-0.1D1 + x1 - t126 + x2 - t140 - t121 + t141)
      t156 = t127 * t155
      t157 = -t115
      t158 = bbggh11J2(s, XB1, XB2, z, lh, wd, t157, x2, x3, x4)
      t161 = t94 * t11 * x2
      t163 = t115 ** 2
      t168 = log(0.4D1 * t161 * t125 * t4 * t136 * t163)
      t170 = bbggh11J1(s, XB1, XB2, z, lh, wd, t157, x2, x3, x4)
      t179 = 0.90D2 * t20 * (-t156 * t158 + t168 * t127 * t155 * t170) +
     # 0.180D3 * t53 * t156 * t170
      t183 = FJET(XB1, XB2, s, t2 * t115 * (-x3 + t116 - t117 + t119 - t
     #120 + t122 - x2 + t133) * t136, t139, -t2 * t115 * t142 * t136, -t
     #147, -s * t148 * x2 * x1 * t115 * t136, t179 * t90 * t74 / 0.2880D
     #4)
      t190 = Sqrt(x2 * t124 * t13)
      t192 = 0.2D1 * t123 * t190
      t198 = 0.1D1 / (0.1D1 - x2 + t121)
      t199 = bbggh11J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t200 = t198 * t199
      t205 = log(0.4D1 * t54 * t11 * t13 * t124)
      t206 = t205 * t198
      t207 = bbggh11J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t212 = bbggh11J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t215 = t205 ** 2
      t222 = t198 * t207
      t231 = log(0.4D1 * t79 * t55 * t4 * t124)
      t243 = -(-0.180D3 * t53 * (t200 - t206 * t207) + 0.90D2 * t20 * (t
     #198 * t212 - t206 * t199 + t215 * t198 * t207 / 0.2D1) + t71 * t22
     #2) * t74 / 0.5760D4 + (0.90D2 * t20 * (-t200 + t231 * t198 * t207)
     # + 0.180D3 * t53 * t222) * t90 * t74 / 0.2880D4
      t244 = FJET(XB1, XB2, s, -t2 * (-x3 + t119 - x2 + t192), 0.0D0, t2
     # * (0.1D1 - x2 - x3 + t119 + t192), 0.0D0, 0.0D0, t243)
      t250 = bbggh11J2(s, XB1, XB2, z, lh, wd, t157, 0.0D0, x3, x4)
      t252 = t13 * t136 * t163
      t255 = log(-0.4D1 * t161 * t252)
      t256 = bbggh11J1(s, XB1, XB2, z, lh, wd, t157, 0.0D0, x3, x4)
      t269 = log(-0.4D1 * t94 * t11 * t252)
      t274 = bbggh11J3(s, XB1, XB2, z, lh, wd, t157, 0.0D0, x3, x4)
      t276 = t269 ** 2
      t286 = (-0.90D2 * t20 * (t250 - t255 * t256) + 0.180D3 * t53 * t25
     #6) * t90 * t74 / 0.2880D4 + (0.180D3 * t53 * (t250 - t269 * t256) 
     #- 0.90D2 * t20 * (t274 - t269 * t250 + t276 * t256 / 0.2D1) - t71 
     #* t256) * t90 / 0.2880D4
      t287 = FJET(XB1, XB2, s, -t2 * t115 * x3, t139, t2 * t115 * t4, -t
     #147, 0.0D0, t286)
      bbggh1n1e1 = t113 * t112 + t183 * t179 * t90 * t74 / 0.2880D4 + t2
     #44 * t243 + t287 * t286

      end function



      doubleprecision function bbggh1n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh11J1
      doubleprecision bbggh11J2
      doubleprecision bbggh11J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = bbggh11J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t9 = x4 * 0.3141592653589793D1
      t10 = Sin(t9)
      t11 = t10 ** 2
      t12 = x2 * t11
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t16 = t14 * x3 * t4
      t19 = log(-0.4D1 * t12 * t16)
      t20 = bbggh11J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t25 = lh * t7
      t27 = 0.180D3 * t25 * t20
      t29 = 0.1D1 / x2
      t33 = 0.1D1 / x1
      t34 = t33 * t29
      t37 = x1 ** 2
      t38 = t37 * t11
      t41 = log(-0.4D1 * t16 * t38)
      t49 = bbggh11J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t54 = x3 * t4
      t57 = log(-0.4D1 * t11 * t14 * t54)
      t65 = lh ** 2
      t67 = 0.3141592653589793D1 ** 2
      t69 = t57 ** 2
      t75 = -(0.90D2 * t7 * (-t8 + t19 * t20) + t27) * t29 / 0.5760D4 + 
     #t7 * t20 * t34 / 0.32D2 + (0.90D2 * t7 * (t8 - t41 * t20) - t27) *
     # t33 / 0.2880D4 + t7 * t49 / 0.64D2 + (-0.180D3 * lh - 0.90D2 * t5
     #7) * t7 * t8 / 0.5760D4 + (0.180D3 * t57 * lh + 0.180D3 * t65 - 0.
     #30D2 * t67 + 0.45D2 * t69) * t7 * t20 / 0.5760D4
      t76 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t7
     #5)
      t78 = -0.1D1 + x1
      t79 = x1 * x3
      t80 = t79 * z
      t82 = 0.2D1 * x2 * x3
      t83 = t79 * x2
      t84 = x2 * z
      t85 = t79 * t84
      t86 = cos(t9)
      t87 = -0.1D1 + x2
      t89 = x1 * z
      t90 = 0.1D1 - x1 + t89
      t94 = Sqrt(x3 * t87 * t90 * x2 * t4)
      t96 = 0.2D1 * t86 * t94
      t99 = 0.1D1 / t90
      t102 = t2 * t79
      t103 = x1 * x2
      t104 = t103 * z
      t105 = 0.1D1 - x1 + t89 - x2 + t103 - t104 - x3 + t79 - t80 + t82 
     #- t83 + t85 + t96
      t110 = t2 * x1 * t4
      t111 = t1 ** 2
      t119 = 0.1D1 / (-0.1D1 + x1 - t89 + x2 - t103 - t84 + t104)
      t121 = -t78
      t122 = bbggh11J1(s, XB1, XB2, z, lh, wd, t121, x2, x3, x4)
      t127 = FJET(XB1, XB2, s, t2 * t78 * (-x3 + t79 - t80 + t82 - t83 +
     # t85 - x2 + t96) * t99, t102, -t2 * t78 * t105 * t99, -t110, -s * 
     #t111 * x2 * x1 * t78 * t99, -t7 * t90 * t119 * t122 * t33 * t29 / 
     #0.32D2)
      t136 = Sqrt(x2 * t87 * t54)
      t138 = 0.2D1 * t86 * t136
      t144 = 0.1D1 / (0.1D1 - x2 + t84)
      t145 = bbggh11J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t151 = log(0.4D1 * t12 * t14 * t54 * t87)
      t153 = bbggh11J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t169 = -(0.90D2 * t7 * (t144 * t145 - t151 * t144 * t153) - 0.180D
     #3 * t25 * t144 * t153) * t29 / 0.5760D4 - t7 * t144 * t153 * t33 *
     # t29 / 0.32D2
      t170 = FJET(XB1, XB2, s, -t2 * (-x3 + t82 - x2 + t138), 0.0D0, t2 
     #* (0.1D1 - x2 - x3 + t82 + t138), 0.0D0, 0.0D0, t169)
      t176 = bbggh11J1(s, XB1, XB2, z, lh, wd, t121, 0.0D0, x3, x4)
      t180 = bbggh11J2(s, XB1, XB2, z, lh, wd, t121, 0.0D0, x3, x4)
      t182 = t78 ** 2
      t187 = log(-0.4D1 * t38 * t14 * t54 * t99 * t182)
      t197 = -t7 * t176 * t34 / 0.32D2 + (-0.90D2 * t7 * (t180 - t187 * 
     #t176) + 0.180D3 * t25 * t176) * t33 / 0.2880D4
      t198 = FJET(XB1, XB2, s, -t2 * t78 * x3, t102, t2 * t78 * t4, -t11
     #0, 0.0D0, t197)
      bbggh1n1e0 = t76 * t75 - t127 * t7 * t90 * t119 * t122 * t34 / 0.3
     #2D2 + t170 * t169 + t198 * t197

      end function



      doubleprecision function bbggh1n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh11J1
      doubleprecision bbggh11J2
      doubleprecision bbggh11J3
      t2 = s * (-0.1D1 + z)
      t4 = -0.1D1 + x3
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = bbggh11J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t12 = x4 * 0.3141592653589793D1
      t13 = Sin(t12)
      t14 = t13 ** 2
      t15 = z ** 2
      t18 = x3 * t4
      t21 = log(-0.4D1 * t14 / t15 * t18)
      t25 = bbggh11J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t28 = t7 * t25
      t29 = 0.1D1 / x2
      t32 = 0.1D1 / x1
      t35 = t7 * t8 / 0.64D2 + (-0.180D3 * lh - 0.90D2 * t21) * t7 * t25
     # / 0.5760D4 + t28 * t29 / 0.64D2 + t28 * t32 / 0.32D2
      t36 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t3
     #5)
      t39 = 0.2D1 * x2 * x3
      t40 = cos(t12)
      t44 = Sqrt(x2 * (-0.1D1 + x2) * t18)
      t46 = 0.2D1 * t40 * t44
      t53 = 0.1D1 / (0.1D1 - x2 + x2 * z)
      t55 = bbggh11J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t59 = FJET(XB1, XB2, s, -t2 * (-x3 + t39 - x2 + t46), 0.0D0, t2 * 
     #(0.1D1 - x2 - x3 + t39 + t46), 0.0D0, 0.0D0, -t7 * t53 * t55 * t29
     # / 0.64D2)
      t65 = -0.1D1 + x1
      t75 = bbggh11J1(s, XB1, XB2, z, lh, wd, -t65, 0.0D0, x3, x4)
      t79 = FJET(XB1, XB2, s, -t2 * t65 * x3, t2 * x1 * x3, t2 * t65 * t
     #4, -t2 * x1 * t4, 0.0D0, -t7 * t75 * t32 / 0.32D2)
      bbggh1n1em1 = t36 * t35 - t59 * t7 * t53 * t55 * t29 / 0.64D2 - t7
     #9 * t7 * t75 * t32 / 0.32D2

      end function



      doubleprecision function bbggh1n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh11J1
      doubleprecision bbggh11J2
      doubleprecision bbggh11J3
      t2 = s * (-0.1D1 + z)
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = bbggh11J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t11 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * (-0.1D1 + x3), 0.0D0
     #, 0.0D0, t7 * t8 / 0.64D2)
      bbggh1n1em2 = t11 * t7 * t8 / 0.64D2

      end function



      doubleprecision function bbggh1n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh11J1
      doubleprecision bbggh11J2
      doubleprecision bbggh11J3
      bbggh1n1em3 = 0.0D0

      end function



      doubleprecision function bbggh1n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh11J1
      doubleprecision bbggh11J2
      doubleprecision bbggh11J3
      bbggh1n1em4 = 0.0D0

      end function


      doubleprecision function bbggh1n2e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh11J1
      doubleprecision bbggh11J2
      doubleprecision bbggh11J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t6 = z ** 2
      t8 = 0.1D1 / t6 / z
      t9 = x4 * 0.3141592653589793D1
      t10 = Sin(t9)
      t11 = t10 ** 2
      t13 = x3 * t4
      t16 = log(-0.4D1 * t8 * t11 * t13)
      t19 = lh ** 2
      t20 = 0.180D3 * t19
      t21 = 0.3141592653589793D1 ** 2
      t22 = 0.30D2 * t21
      t23 = t16 ** 2
      t26 = s ** 2
      t27 = 0.1D1 / t26
      t29 = bbggh11J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t32 = t20 - t22
      t44 = bbggh11J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t47 = t27 * lh
      t48 = bbggh11J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t49 = x2 * t8
      t51 = -0.1D1 + x2
      t55 = log(0.4D1 * t49 * t11 * t13 * t51)
      t56 = bbggh11J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t62 = log(-0.4D1 * t49 * t11 * x3 * t4)
      t67 = bbggh11J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t69 = t62 ** 2
      t72 = bbggh11J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t74 = t55 ** 2
      t80 = t32 * t27
      t81 = t56 - t44
      t84 = 0.1D1 / x2
      t93 = x1 ** 2
      t94 = x2 * t93
      t95 = t94 * t11
      t96 = t8 * x3
      t101 = log(0.4D1 * t95 * t96 * t4 * t51)
      t103 = t96 * t4
      t106 = log(-0.4D1 * t95 * t103)
      t114 = 0.1D1 / x1
      t118 = t93 * t11
      t121 = log(-0.4D1 * t118 * t103)
      t127 = t121 ** 2
      t137 = (0.180D3 * t16 * lh + t20 - t22 + 0.45D2 * t23) * t27 * t29
     # / 0.5760D4 + (-t16 * t32 - 0.90D2 * t23 * lh - 0.2884936567583026
     #D3 - 0.120D3 * t19 * lh + 0.60D2 * lh * t21 - 0.15D2 * t23 * t16) 
     #* t27 * t44 / 0.5760D4 - (-0.180D3 * t47 * (t48 - t55 * t56 - t29 
     #+ t62 * t44) + 0.90D2 * t27 * (-t67 + t62 * t29 - t69 * t44 / 0.2D
     #1 + t72 - t55 * t48 + t74 * t56 / 0.2D1) + t80 * t81) * t84 / 0.57
     #60D4 + (-0.180D3 * lh - 0.90D2 * t16) * t27 * t67 / 0.5760D4 - (0.
     #90D2 * t27 * (t48 - t101 * t56 - t29 + t106 * t44) - 0.180D3 * t47
     # * t81) * t114 * t84 / 0.2880D4 + (-0.180D3 * t47 * (t29 - t121 * 
     #t44) + 0.90D2 * t27 * (t67 - t121 * t29 + t127 * t44 / 0.2D1) + t8
     #0 * t44) * t114 / 0.2880D4
      t138 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t
     #137)
      t140 = x1 * x3
      t142 = -0.1D1 + x1
      t144 = t2 * t142 * x3
      t148 = t2 * t142 * t4
      t149 = bbggh11J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t150 = 0.1D1 / t6
      t152 = t94 * t11 * t150
      t153 = x1 * z
      t154 = -z - x1 + t153
      t155 = 0.1D1 / t154
      t156 = t142 ** 2
      t157 = t155 * t156
      t158 = t13 * t157
      t161 = log(0.4D1 * t152 * t158)
      t162 = bbggh11J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t175 = log(0.4D1 * t118 * t150 * t158)
      t180 = bbggh11J3(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t182 = t175 ** 2
      t192 = -(0.90D2 * t27 * (t149 - t161 * t162) - 0.180D3 * t47 * t16
     #2) * t114 * t84 / 0.2880D4 + (0.180D3 * t47 * (t149 - t175 * t162)
     # - 0.90D2 * t27 * (t180 - t175 * t149 + t182 * t162 / 0.2D1) - t80
     # * t162) * t114 / 0.2880D4
      t193 = FJET(XB1, XB2, s, t2 * t140, -t144, -t2 * x1 * t4, t148, 0.
     #0D0, t192)
      t195 = x3 * z
      t196 = t140 * z
      t197 = x2 * x3
      t198 = t197 * z
      t199 = t140 * x2
      t200 = x2 * z
      t201 = t140 * t200
      t202 = cos(t9)
      t207 = Sqrt(-x3 * t51 * t154 * x2 * t4)
      t209 = 0.2D1 * t202 * t207
      t214 = x1 * x2
      t215 = t214 * z
      t216 = z + x1 - t153 - t200 - t214 + t215 - t195 - t140 + t196 + t
     #198 + t199 - t201 + t197 + t209
      t220 = t1 ** 2
      t227 = 0.1D1 / (z + x1 - t153 - t214 + t215)
      t228 = t154 * t227
      t229 = bbggh11J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t235 = log(-0.4D1 * t152 * t13 * t157 * t51)
      t237 = bbggh11J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t246 = 0.90D2 * t27 * (t228 * t229 - t235 * t154 * t227 * t237) - 
     #0.180D3 * t47 * t228 * t237
      t250 = FJET(XB1, XB2, s, t2 * x1 * (-t195 - t140 + t196 + t198 + t
     #199 - t201 - x2 + t197 + t209) * t155, -t144, -t2 * x1 * t216 * t1
     #55, t148, s * t220 * x2 * x1 * t142 * t155, -t246 * t114 * t84 / 0
     #.2880D4)
      bbggh1n2e1 = t138 * t137 + t193 * t192 - t250 * t246 * t114 * t84 
     #/ 0.2880D4

      end function



      doubleprecision function bbggh1n2e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh11J1
      doubleprecision bbggh11J2
      doubleprecision bbggh11J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = bbggh11J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t9 = z ** 2
      t11 = 0.1D1 / t9 / z
      t12 = x2 * t11
      t13 = x4 * 0.3141592653589793D1
      t14 = Sin(t13)
      t15 = t14 ** 2
      t17 = x3 * t4
      t18 = -0.1D1 + x2
      t22 = log(0.4D1 * t12 * t15 * t17 * t18)
      t23 = bbggh11J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t25 = bbggh11J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t30 = log(-0.4D1 * t12 * t15 * x3 * t4)
      t31 = bbggh11J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t36 = lh * t7
      t37 = t23 - t31
      t41 = 0.1D1 / x2
      t45 = 0.1D1 / x1
      t46 = t45 * t41
      t49 = x1 ** 2
      t50 = t49 * t15
      t55 = log(-0.4D1 * t50 * t11 * x3 * t4)
      t65 = bbggh11J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t72 = log(-0.4D1 * t11 * t15 * t17)
      t80 = lh ** 2
      t82 = 0.3141592653589793D1 ** 2
      t84 = t72 ** 2
      t90 = -(0.90D2 * t7 * (t8 - t22 * t23 - t25 + t30 * t31) - 0.180D3
     # * t36 * t37) * t41 / 0.5760D4 - t7 * t37 * t46 / 0.32D2 + (0.90D2
     # * t7 * (t25 - t55 * t31) - 0.180D3 * t36 * t31) * t45 / 0.2880D4 
     #+ t7 * t65 / 0.64D2 + (-0.180D3 * lh - 0.90D2 * t72) * t7 * t25 / 
     #0.5760D4 + (0.180D3 * t72 * lh + 0.180D3 * t80 - 0.30D2 * t82 + 0.
     #45D2 * t84) * t7 * t31 / 0.5760D4
      t91 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t9
     #0)
      t93 = x1 * x3
      t95 = -0.1D1 + x1
      t97 = t2 * t95 * x3
      t101 = t2 * t95 * t4
      t102 = bbggh11J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t106 = bbggh11J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t109 = x1 * z
      t110 = -z - x1 + t109
      t111 = 0.1D1 / t110
      t112 = t95 ** 2
      t117 = log(0.4D1 * t50 / t9 * t17 * t111 * t112)
      t127 = -t7 * t102 * t46 / 0.32D2 + (-0.90D2 * t7 * (t106 - t117 * 
     #t102) + 0.180D3 * t36 * t102) * t45 / 0.2880D4
      t128 = FJET(XB1, XB2, s, t2 * t93, -t97, -t2 * x1 * t4, t101, 0.0D
     #0, t127)
      t130 = x3 * z
      t131 = t93 * z
      t132 = x2 * x3
      t133 = t132 * z
      t134 = x2 * t93
      t135 = x2 * z
      t136 = t93 * t135
      t137 = cos(t13)
      t142 = Sqrt(-x3 * t18 * t110 * x2 * t4)
      t144 = 0.2D1 * t137 * t142
      t149 = x1 * x2
      t150 = t149 * z
      t151 = z + x1 - t109 - t135 - t149 + t150 - t130 - t93 + t131 + t1
     #33 + t134 - t136 + t132 + t144
      t155 = t1 ** 2
      t163 = 0.1D1 / (z + x1 - t109 - t149 + t150)
      t165 = bbggh11J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t170 = FJET(XB1, XB2, s, t2 * x1 * (-t130 - t93 + t131 + t133 + t1
     #34 - t136 - x2 + t132 + t144) * t111, -t97, -t2 * x1 * t151 * t111
     #, t101, s * t155 * x2 * x1 * t95 * t111, -t7 * t110 * t163 * t165 
     #* t45 * t41 / 0.32D2)
      bbggh1n2e0 = t91 * t90 + t128 * t127 - t170 * t7 * t110 * t163 * t
     #165 * t46 / 0.32D2

      end function



      doubleprecision function bbggh1n2em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh11J1
      doubleprecision bbggh11J2
      doubleprecision bbggh11J3
      t2 = s * (-0.1D1 + z)
      t4 = -0.1D1 + x3
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = bbggh11J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t12 = z ** 2
      t16 = Sin(x4 * 0.3141592653589793D1)
      t17 = t16 ** 2
      t22 = log(-0.4D1 / t12 / z * t17 * x3 * t4)
      t26 = bbggh11J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t29 = bbggh11J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t36 = 0.1D1 / x1
      t39 = t7 * t8 / 0.64D2 + (-0.180D3 * lh - 0.90D2 * t22) * t7 * t26
     # / 0.5760D4 - t7 * (t29 - t26) / x2 / 0.64D2 + t7 * t26 * t36 / 0.
     #32D2
      t40 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t3
     #9)
      t44 = -0.1D1 + x1
      t51 = bbggh11J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t55 = FJET(XB1, XB2, s, t2 * x1 * x3, -t2 * t44 * x3, -t2 * x1 * t
     #4, t2 * t44 * t4, 0.0D0, -t7 * t51 * t36 / 0.32D2)
      bbggh1n2em1 = t40 * t39 - t55 * t7 * t51 * t36 / 0.32D2

      end function



      doubleprecision function bbggh1n2em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh11J1
      doubleprecision bbggh11J2
      doubleprecision bbggh11J3
      t2 = s * (-0.1D1 + z)
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = bbggh11J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t11 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * (-0.1D1 + x3)
     #, 0.0D0, t7 * t8 / 0.64D2)
      bbggh1n2em2 = t11 * t7 * t8 / 0.64D2

      end function



      doubleprecision function bbggh1n2em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh11J1
      doubleprecision bbggh11J2
      doubleprecision bbggh11J3
      bbggh1n2em3 = 0.0D0

      end function



      doubleprecision function bbggh1n2em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh11J1
      doubleprecision bbggh11J2
      doubleprecision bbggh11J3
      bbggh1n2em4 = 0.0D0

      end function
  
 

      doubleprecision function bbggh11J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = 0.1D1 - z
      t3 = t2 ** 2
      t4 = t1 * t3
      t5 = 0.1D1 - x1
      t6 = t5 ** 2
      t7 = 0.1D1 - x3
      t8 = t7 ** 2
      t12 = x1 ** 2
      t14 = z + x1 * t2
      t15 = t14 ** 2
      t16 = 0.1D1 / t15
      t18 = 0.1D1 - x2
      t23 = cos(x4 * 0.3141592653589793D1)
      t24 = x3 * t18
      t28 = Sqrt(t24 * t14 * x2 * t7)
      t30 = 0.2D1 * t23 * t28
      t31 = t7 * t18 * t14 + x2 * x3 + t30
      t32 = t31 ** 2
      t36 = t1 * t2
      t40 = z + x1 * t18 * t2
      t41 = 0.1D1 / t14
      t42 = t40 * t41
      t47 = t1 * t3 * t2
      t48 = t47 * t6
      t49 = x2 * t7
      t50 = x1 * t41
      t54 = t47 * t12
      t56 = x2 * t5
      t67 = t1 * z
      t69 = x1 * x2
      t82 = t4 * t5
      t88 = t3 ** 2
      t90 = x2 ** 2
      t96 = z ** 2
      t99 = t40 * t16
      t103 = t4 * t6
      t114 = t12 * t41
      t126 = -0.36D2 * t4 * t6 * t8 - 0.36D2 * t4 * t12 * t16 * t32 - 0.
     #72D2 * t36 * t5 * t42 * z + 0.72D2 * t48 * t49 * t50 + 0.72D2 * t5
     #4 * t16 * t31 * t56 + 0.36D2 * t4 * x1 * t5 * t7 - 0.72D2 * t36 * 
     #x1 * z - 0.112D3 * t67 * t3 * t69 * t5 * t41 + 0.36D2 * t67 * t2 *
     # t5 * t7 + 0.36D2 * t67 * t2 * t50 * t31 - 0.72D2 * t82 * t7 * x1 
     #* t41 * t31 - 0.58D2 * t1 * t88 * t90 * t12 * t6 * t16 - 0.144D3 *
     # t1 * t96 - 0.45D2 * t48 * t99 * t69 + 0.18D2 * t103 * t42 * t7 + 
     #0.36D2 * t82 * t99 * x1 * t31 - 0.18D2 * t103 * t42 * x3 + 0.18D2 
     #* t4 * t114 * t31 - 0.18D2 * t4 * t114 * (t24 * t14 + t49 - t30) -
     # 0.45D2 * t54 * t56 * t41
      bbggh11J1 = 0.16D2 / 0.3D1 * wd * t126

      end function
  
   
 

      doubleprecision function bbggh11J2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = 0.1D1 - z
      t3 = t2 ** 2
      t4 = t1 * t3
      t5 = 0.1D1 - x1
      t6 = t5 ** 2
      t7 = 0.1D1 - x3
      t8 = t7 ** 2
      t12 = t4 * t5
      t15 = z + x1 * t2
      t16 = 0.1D1 / t15
      t17 = 0.1D1 - x2
      t22 = cos(x4 * 0.3141592653589793D1)
      t23 = x3 * t17
      t27 = Sqrt(t23 * t15 * x2 * t7)
      t29 = 0.2D1 * t22 * t27
      t30 = t7 * t17 * t15 + x2 * x3 + t29
      t35 = x1 ** 2
      t36 = t35 * t16
      t38 = x2 * t7
      t43 = t3 ** 2
      t45 = x2 ** 2
      t48 = t15 ** 2
      t49 = 0.1D1 / t48
      t58 = t1 * t3 * t2
      t59 = t58 * t6
      t62 = z + x1 * t17 * t2
      t63 = t62 * t49
      t64 = x1 * x2
      t68 = t58 * t35
      t69 = x2 * t5
      t74 = t30 ** 2
      t82 = t4 * t6
      t83 = t62 * t16
      t93 = t1 * z
      t103 = x1 * t16
      t115 = 0.36D2 * t4 * t6 * t8 + 0.72D2 * t12 * t7 * x1 * t16 * t30 
     #+ 0.18D2 * t4 * t36 * (t23 * t15 + t38 - t29) + 0.58D2 * t1 * t43 
     #* t45 * t35 * t6 * t49 - 0.36D2 * t4 * x1 * t5 * t7 + 0.52D2 * t59
     # * t63 * t64 + 0.52D2 * t68 * t69 * t16 + 0.36D2 * t4 * t35 * t49 
     #* t74 - 0.36D2 * t12 * t63 * x1 * t30 - 0.18D2 * t82 * t83 * t7 + 
     #0.18D2 * t82 * t83 * x3 - 0.18D2 * t4 * t36 * t30 + 0.40D2 * t93 *
     # t3 * t64 * t5 * t16 - 0.36D2 * t93 * t2 * t5 * t7 - 0.72D2 * t59 
     #* t38 * t103 - 0.72D2 * t68 * t49 * t30 * t69 - 0.36D2 * t93 * t2 
     #* t103 * t30
      bbggh11J2 = 0.16D2 / 0.3D1 * wd * t115

      end function
  
   
 

      doubleprecision function bbggh11J3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = 0.1D1 - z
      t3 = t2 ** 2
      t4 = t3 ** 2
      t6 = x2 ** 2
      t8 = x1 ** 2
      t9 = 0.1D1 - x1
      t10 = t9 ** 2
      t13 = z + x1 * t2
      t14 = t13 ** 2
      t15 = 0.1D1 / t14
      t20 = t1 * t3 * t2
      t23 = 0.1D1 / t13
      t29 = x1 * x2
      bbggh11J3 = 0.16D2 / 0.3D1 * wd * (0.36D2 * t1 * t4 * t6 * t8 * t1
     #0 * t15 + 0.18D2 * t20 * t8 * x2 * t9 * t23 + 0.36D2 * t1 * z * t3
     # * t29 * t9 * t23 + 0.18D2 * t20 * t10 * (z + x1 * (0.1D1 - x2) * 
     #t2) * t15 * t29)

      end function
  
 