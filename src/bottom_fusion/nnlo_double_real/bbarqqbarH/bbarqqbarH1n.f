  
      subroutine bbarqqbarH1n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision bbarqqbarH11J1  
      doubleprecision bbarqqbarH11J2  
      doubleprecision bbarqqbarH1n1e1  
      doubleprecision bbarqqbarH1n1e0  
      doubleprecision bbarqqbarH1n1em1  
      doubleprecision bbarqqbarH1n1em2  
      doubleprecision bbarqqbarH1n1em3  
      doubleprecision bbarqqbarH1n1em4  
      doubleprecision bbarqqbarH1n2e1  
      doubleprecision bbarqqbarH1n2e0  
      doubleprecision bbarqqbarH1n2em1  
      doubleprecision bbarqqbarH1n2em2  
      doubleprecision bbarqqbarH1n2em3  
      doubleprecision bbarqqbarH1n2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=bbarqqbarH1n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarqqbarH1n2e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=bbarqqbarH1n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarqqbarH1n2e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=bbarqqbarH1n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarqqbarH1n2em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=bbarqqbarH1n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarqqbarH1n2em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=bbarqqbarH1n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarqqbarH1n2em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=bbarqqbarH1n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbarqqbarH1n2em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function bbarqqbarH1n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarqqbarH11J1
      doubleprecision bbarqqbarH11J2
      t1 = -0.1D1 + z
      t2 = t1 * s
      t4 = -0.1D1 + x3
      t6 = x4 * 0.3141592653589793D1
      t7 = Sin(t6)
      t8 = t7 ** 2
      t9 = z ** 2
      t10 = 0.1D1 / t9
      t11 = t8 * t10
      t12 = x3 * t4
      t15 = log(-0.4D1 * t11 * t12)
      t18 = lh ** 2
      t19 = 0.180D3 * t18
      t20 = 0.3141592653589793D1 ** 2
      t21 = 0.30D2 * t20
      t22 = t15 ** 2
      t25 = s ** 2
      t26 = 0.1D1 / t25
      t28 = bbarqqbarH11J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4
     #)
      t31 = t19 - t21
      t43 = bbarqqbarH11J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4
     #)
      t46 = lh * t26
      t49 = t10 * x3 * t4
      t52 = log(-0.4D1 * x2 * t8 * t49)
      t58 = t52 ** 2
      t64 = t31 * t26
      t65 = t64 * t43
      t67 = 0.1D1 / x2
      t70 = x1 ** 2
      t75 = log(-0.4D1 * x2 * t70 * t8 * t49)
      t84 = 0.1D1 / x1
      t87 = t70 * t8
      t90 = log(-0.4D1 * t87 * t49)
      t96 = t90 ** 2
      t105 = (0.180D3 * t15 * lh + t19 - t21 + 0.45D2 * t22) * t26 * t28
     # / 0.5760D4 + (-t15 * t31 - 0.90D2 * t22 * lh + 0.60D2 * lh * t20 
     #- 0.2884936567583026D3 - 0.120D3 * t18 * lh - 0.15D2 * t22 * t15) 
     #* t26 * t43 / 0.5760D4 - (-0.180D3 * t46 * (-t28 + t52 * t43) + 0.
     #90D2 * t26 * (t52 * t28 - t58 * t43 / 0.2D1) - t65) * t67 / 0.5760
     #D4 - (0.90D2 * t26 * (-t28 + t75 * t43) + 0.180D3 * t46 * t43) * t
     #67 * t84 / 0.2880D4 + (-0.180D3 * t46 * (t28 - t90 * t43) + 0.90D2
     # * t26 * (-t90 * t28 + t96 * t43 / 0.2D1) + t65) * t84 / 0.2880D4
      t106 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t
     #105)
      t108 = -0.1D1 + x1
      t109 = x3 * x1
      t110 = t109 * z
      t111 = x2 * x3
      t112 = 0.2D1 * t111
      t113 = t111 * x1
      t114 = x1 * z
      t115 = t111 * t114
      t116 = cos(t6)
      t117 = -0.1D1 + x2
      t118 = x3 * t117
      t119 = 0.1D1 - x1 + t114
      t123 = Sqrt(t118 * t119 * x2 * t4)
      t125 = 0.2D1 * t116 * t123
      t128 = 0.1D1 / t119
      t131 = t2 * t109
      t132 = x2 * x1
      t133 = t132 * z
      t134 = 0.1D1 - x1 + t114 - x2 + t132 - t133 - x3 + t109 - t110 + t
     #112 - t113 + t115 + t125
      t139 = t2 * x1 * t4
      t140 = t1 ** 2
      t146 = x2 * z
      t148 = 0.1D1 / (x2 - t132 - 0.1D1 + x1 - t114 - t146 + t133)
      t149 = t119 * t148
      t150 = -t108
      t151 = bbarqqbarH11J2(s, XB1, XB2, z, lh, wd, t150, x2, x3, x4)
      t154 = t87 * t10 * x2
      t156 = t108 ** 2
      t161 = log(0.4D1 * t154 * t118 * t4 * t128 * t156)
      t163 = bbarqqbarH11J1(s, XB1, XB2, z, lh, wd, t150, x2, x3, x4)
      t172 = 0.90D2 * t26 * (t149 * t151 - t161 * t119 * t148 * t163) - 
     #0.180D3 * t46 * t149 * t163
      t176 = FJET(XB1, XB2, s, t2 * t108 * (-x3 + t109 - t110 + t112 - t
     #113 + t115 - x2 + t125) * t128, t131, -t2 * t108 * t134 * t128, -t
     #139, -s * t140 * x2 * x1 * t108 * t128, -t172 * t67 * t84 / 0.2880
     #D4)
      t182 = x2 * t117 * t12
      t183 = Sqrt(t182)
      t185 = 0.2D1 * t116 * t183
      t191 = 0.1D1 / (-x2 + 0.1D1 + t146)
      t192 = bbarqqbarH11J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t193 = t191 * t192
      t198 = log(0.4D1 * t11 * x2 * t118 * t4)
      t199 = t198 * t191
      t200 = bbarqqbarH11J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t206 = t198 ** 2
      t213 = t191 * t200
      t218 = t87 * t10
      t221 = log(0.4D1 * t218 * t182)
      t233 = -(-0.180D3 * t46 * (t193 - t199 * t200) + 0.90D2 * t26 * (-
     #t199 * t192 + t206 * t191 * t200 / 0.2D1) + t64 * t213) * t67 / 0.
     #5760D4 - (0.90D2 * t26 * (t193 - t221 * t191 * t200) - 0.180D3 * t
     #46 * t213) * t67 * t84 / 0.2880D4
      t234 = FJET(XB1, XB2, s, -t2 * (-x3 + t112 - x2 + t185), 0.0D0, t2
     # * (0.1D1 - x2 - x3 + t112 + t185), 0.0D0, 0.0D0, t233)
      t240 = bbarqqbarH11J2(s, XB1, XB2, z, lh, wd, t150, 0.0D0, x3, x4)
      t242 = t12 * t128 * t156
      t245 = log(-0.4D1 * t154 * t242)
      t246 = bbarqqbarH11J1(s, XB1, XB2, z, lh, wd, t150, 0.0D0, x3, x4)
      t258 = log(-0.4D1 * t218 * t242)
      t264 = t258 ** 2
      t274 = -(0.90D2 * t26 * (t240 - t245 * t246) - 0.180D3 * t46 * t24
     #6) * t67 * t84 / 0.2880D4 + (0.180D3 * t46 * (t240 - t258 * t246) 
     #- 0.90D2 * t26 * (-t258 * t240 + t264 * t246 / 0.2D1) - t64 * t246
     #) * t84 / 0.2880D4
      t275 = FJET(XB1, XB2, s, -t2 * t108 * x3, t131, t2 * t108 * t4, -t
     #139, 0.0D0, t274)
      bbarqqbarH1n1e1 = t106 * t105 - t176 * t172 * t67 * t84 / 0.2880D4
     # + t234 * t233 + t275 * t274

      end function



      doubleprecision function bbarqqbarH1n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarqqbarH11J1
      doubleprecision bbarqqbarH11J2
      t1 = -0.1D1 + z
      t2 = t1 * s
      t4 = -0.1D1 + x3
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = bbarqqbarH11J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t9 = x4 * 0.3141592653589793D1
      t10 = Sin(t9)
      t11 = t10 ** 2
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t16 = t14 * x3 * t4
      t19 = log(-0.4D1 * x2 * t11 * t16)
      t20 = bbarqqbarH11J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4
     #)
      t25 = lh * t7
      t27 = 0.180D3 * t25 * t20
      t29 = 0.1D1 / x2
      t33 = 0.1D1 / x1
      t34 = t29 * t33
      t37 = x1 ** 2
      t38 = t37 * t11
      t41 = log(-0.4D1 * t38 * t16)
      t50 = t11 * t14
      t51 = x3 * t4
      t54 = log(-0.4D1 * t50 * t51)
      t62 = lh ** 2
      t64 = 0.3141592653589793D1 ** 2
      t66 = t54 ** 2
      t72 = -(0.90D2 * t7 * (-t8 + t19 * t20) + t27) * t29 / 0.5760D4 + 
     #t7 * t20 * t34 / 0.32D2 + (0.90D2 * t7 * (t8 - t41 * t20) - t27) *
     # t33 / 0.2880D4 + (-0.180D3 * lh - 0.90D2 * t54) * t7 * t8 / 0.576
     #0D4 + (0.180D3 * t54 * lh + 0.180D3 * t62 - 0.30D2 * t64 + 0.45D2 
     #* t66) * t7 * t20 / 0.5760D4
      t73 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t7
     #2)
      t75 = -0.1D1 + x1
      t76 = x3 * x1
      t77 = t76 * z
      t78 = x2 * x3
      t79 = 0.2D1 * t78
      t80 = t78 * x1
      t81 = x1 * z
      t82 = t78 * t81
      t83 = cos(t9)
      t84 = -0.1D1 + x2
      t85 = x3 * t84
      t86 = 0.1D1 - x1 + t81
      t90 = Sqrt(t85 * t86 * x2 * t4)
      t92 = 0.2D1 * t83 * t90
      t95 = 0.1D1 / t86
      t98 = t2 * t76
      t99 = x2 * x1
      t100 = t99 * z
      t101 = 0.1D1 - x1 + t81 - x2 + t99 - t100 - x3 + t76 - t77 + t79 -
     # t80 + t82 + t92
      t106 = t2 * x1 * t4
      t107 = t1 ** 2
      t114 = x2 * z
      t116 = 0.1D1 / (x2 - t99 - 0.1D1 + x1 - t81 - t114 + t100)
      t118 = -t75
      t119 = bbarqqbarH11J1(s, XB1, XB2, z, lh, wd, t118, x2, x3, x4)
      t124 = FJET(XB1, XB2, s, t2 * t75 * (-x3 + t76 - t77 + t79 - t80 +
     # t82 - x2 + t92) * t95, t98, -t2 * t75 * t101 * t95, -t106, -s * t
     #107 * x2 * x1 * t75 * t95, -t7 * t86 * t116 * t119 * t29 * t33 / 0
     #.32D2)
      t133 = Sqrt(x2 * t84 * t51)
      t135 = 0.2D1 * t83 * t133
      t141 = 0.1D1 / (-x2 + 0.1D1 + t114)
      t142 = bbarqqbarH11J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t148 = log(0.4D1 * t50 * x2 * t85 * t4)
      t150 = bbarqqbarH11J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t166 = -(0.90D2 * t7 * (t141 * t142 - t148 * t141 * t150) - 0.180D
     #3 * t25 * t141 * t150) * t29 / 0.5760D4 - t7 * t141 * t150 * t29 *
     # t33 / 0.32D2
      t167 = FJET(XB1, XB2, s, -t2 * (-x3 + t79 - x2 + t135), 0.0D0, t2 
     #* (0.1D1 - x2 - x3 + t79 + t135), 0.0D0, 0.0D0, t166)
      t173 = bbarqqbarH11J1(s, XB1, XB2, z, lh, wd, t118, 0.0D0, x3, x4)
      t177 = bbarqqbarH11J2(s, XB1, XB2, z, lh, wd, t118, 0.0D0, x3, x4)
      t179 = t75 ** 2
      t184 = log(-0.4D1 * t38 * t14 * t51 * t95 * t179)
      t194 = -t7 * t173 * t34 / 0.32D2 + (-0.90D2 * t7 * (t177 - t184 * 
     #t173) + 0.180D3 * t25 * t173) * t33 / 0.2880D4
      t195 = FJET(XB1, XB2, s, -t2 * t75 * x3, t98, t2 * t75 * t4, -t106
     #, 0.0D0, t194)
      bbarqqbarH1n1e0 = t73 * t72 - t124 * t7 * t86 * t116 * t119 * t34 
     #/ 0.32D2 + t167 * t166 + t195 * t194

      end function



      doubleprecision function bbarqqbarH1n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarqqbarH11J1
      doubleprecision bbarqqbarH11J2
      t2 = (-0.1D1 + z) * s
      t4 = -0.1D1 + x3
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = bbarqqbarH11J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t12 = x4 * 0.3141592653589793D1
      t13 = Sin(t12)
      t14 = t13 ** 2
      t15 = z ** 2
      t18 = x3 * t4
      t21 = log(-0.4D1 * t14 / t15 * t18)
      t25 = bbarqqbarH11J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4
     #)
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
      t53 = 0.1D1 / (-x2 + 0.1D1 + x2 * z)
      t55 = bbarqqbarH11J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t59 = FJET(XB1, XB2, s, -t2 * (-x3 + t39 - x2 + t46), 0.0D0, t2 * 
     #(0.1D1 - x2 - x3 + t39 + t46), 0.0D0, 0.0D0, -t7 * t53 * t55 * t29
     # / 0.64D2)
      t65 = -0.1D1 + x1
      t75 = bbarqqbarH11J1(s, XB1, XB2, z, lh, wd, -t65, 0.0D0, x3, x4)
      t79 = FJET(XB1, XB2, s, -t2 * t65 * x3, t2 * x1 * x3, t2 * t65 * t
     #4, -t2 * x1 * t4, 0.0D0, -t7 * t75 * t32 / 0.32D2)
      bbarqqbarH1n1em1 = t36 * t35 - t59 * t7 * t53 * t55 * t29 / 0.64D2
     # - t79 * t7 * t75 * t32 / 0.32D2

      end function



      doubleprecision function bbarqqbarH1n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarqqbarH11J1
      doubleprecision bbarqqbarH11J2
      t2 = (-0.1D1 + z) * s
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = bbarqqbarH11J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t11 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * (-0.1D1 + x3), 0.0D0
     #, 0.0D0, t7 * t8 / 0.64D2)
      bbarqqbarH1n1em2 = t11 * t7 * t8 / 0.64D2

      end function



      doubleprecision function bbarqqbarH1n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarqqbarH11J1
      doubleprecision bbarqqbarH11J2
      bbarqqbarH1n1em3 = 0.0D0

      end function



      doubleprecision function bbarqqbarH1n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarqqbarH11J1
      doubleprecision bbarqqbarH11J2
      bbarqqbarH1n1em4 = 0.0D0

      end function


      doubleprecision function bbarqqbarH1n2e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarqqbarH11J1
      doubleprecision bbarqqbarH11J2
      t1 = -0.1D1 + z
      t2 = t1 * s
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
      t29 = bbarqqbarH11J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t32 = t20 - t22
      t44 = bbarqqbarH11J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t47 = lh * t27
      t48 = bbarqqbarH11J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t49 = x2 * t8
      t51 = -0.1D1 + x2
      t52 = t13 * t51
      t55 = log(0.4D1 * t49 * t11 * t52)
      t56 = bbarqqbarH11J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t62 = log(-0.4D1 * t49 * t11 * x3 * t4)
      t68 = t62 ** 2
      t72 = t55 ** 2
      t78 = t32 * t27
      t79 = t56 - t44
      t82 = 0.1D1 / x2
      t85 = x1 ** 2
      t86 = x2 * t85
      t87 = t86 * t11
      t88 = t8 * x3
      t93 = log(0.4D1 * t87 * t88 * t4 * t51)
      t95 = t88 * t4
      t98 = log(-0.4D1 * t87 * t95)
      t107 = 0.1D1 / x1
      t110 = t85 * t11
      t113 = log(-0.4D1 * t110 * t95)
      t119 = t113 ** 2
      t129 = (0.180D3 * t16 * lh + t20 - t22 + 0.45D2 * t23) * t27 * t29
     # / 0.5760D4 + (-t16 * t32 - 0.90D2 * t23 * lh + 0.60D2 * lh * t21 
     #- 0.2884936567583026D3 - 0.120D3 * t19 * lh - 0.15D2 * t23 * t16) 
     #* t27 * t44 / 0.5760D4 - (-0.180D3 * t47 * (t48 - t55 * t56 - t29 
     #+ t62 * t44) + 0.90D2 * t27 * (t62 * t29 - t68 * t44 / 0.2D1 - t55
     # * t48 + t72 * t56 / 0.2D1) + t78 * t79) * t82 / 0.5760D4 - (0.90D
     #2 * t27 * (t48 - t93 * t56 - t29 + t98 * t44) - 0.180D3 * t47 * t7
     #9) * t82 * t107 / 0.2880D4 + (-0.180D3 * t47 * (t29 - t113 * t44) 
     #+ 0.90D2 * t27 * (-t113 * t29 + t119 * t44 / 0.2D1) + t78 * t44) *
     # t107 / 0.2880D4
      t130 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t
     #129)
      t132 = x3 * x1
      t134 = -0.1D1 + x1
      t136 = t2 * t134 * x3
      t140 = t2 * t134 * t4
      t141 = bbarqqbarH11J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t142 = 0.1D1 / t6
      t145 = x1 * z
      t146 = -z - x1 + t145
      t147 = 0.1D1 / t146
      t148 = t134 ** 2
      t150 = t13 * t147 * t148
      t153 = log(0.4D1 * t86 * t11 * t142 * t150)
      t154 = bbarqqbarH11J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t167 = log(0.4D1 * t110 * t142 * t150)
      t173 = t167 ** 2
      t183 = -(0.90D2 * t27 * (t141 - t153 * t154) - 0.180D3 * t47 * t15
     #4) * t82 * t107 / 0.2880D4 + (0.180D3 * t47 * (t141 - t167 * t154)
     # - 0.90D2 * t27 * (-t167 * t141 + t173 * t154 / 0.2D1) - t78 * t15
     #4) * t107 / 0.2880D4
      t184 = FJET(XB1, XB2, s, t2 * t132, -t136, -t2 * x1 * t4, t140, 0.
     #0D0, t183)
      t186 = x3 * z
      t187 = t132 * z
      t188 = x2 * x3
      t189 = t188 * z
      t190 = t188 * x1
      t191 = t188 * t145
      t192 = cos(t9)
      t197 = Sqrt(-x3 * t51 * t146 * x2 * t4)
      t199 = 0.2D1 * t192 * t197
      t205 = x2 * x1
      t206 = t205 * z
      t207 = z + x1 - t145 - x2 * z - t205 + t206 - t186 - t132 + t187 +
     # t189 + t190 - t191 + t188 + t199
      t211 = t1 ** 2
      t218 = 0.1D1 / (z - t205 + x1 - t145 + t206)
      t219 = t146 * t218
      t220 = bbarqqbarH11J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t229 = log(-0.4D1 * t147 * t11 * t142 * t85 * t148 * x2 * t52)
      t231 = bbarqqbarH11J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t240 = 0.90D2 * t27 * (t219 * t220 - t229 * t146 * t218 * t231) - 
     #0.180D3 * t47 * t219 * t231
      t244 = FJET(XB1, XB2, s, t2 * x1 * (-t186 - t132 + t187 + t189 + t
     #190 - t191 - x2 + t188 + t199) * t147, -t136, -t2 * x1 * t207 * t1
     #47, t140, s * t211 * x2 * x1 * t134 * t147, -t240 * t82 * t107 / 0
     #.2880D4)
      bbarqqbarH1n2e1 = t130 * t129 + t184 * t183 - t244 * t240 * t82 * 
     #t107 / 0.2880D4

      end function



      doubleprecision function bbarqqbarH1n2e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarqqbarH11J1
      doubleprecision bbarqqbarH11J2
      t1 = -0.1D1 + z
      t2 = t1 * s
      t4 = -0.1D1 + x3
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = bbarqqbarH11J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t9 = z ** 2
      t11 = 0.1D1 / t9 / z
      t12 = x2 * t11
      t13 = x4 * 0.3141592653589793D1
      t14 = Sin(t13)
      t15 = t14 ** 2
      t17 = x3 * t4
      t18 = -0.1D1 + x2
      t22 = log(0.4D1 * t12 * t15 * t17 * t18)
      t23 = bbarqqbarH11J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t25 = bbarqqbarH11J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t30 = log(-0.4D1 * t12 * t15 * x3 * t4)
      t31 = bbarqqbarH11J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t36 = lh * t7
      t37 = t23 - t31
      t41 = 0.1D1 / x2
      t45 = 0.1D1 / x1
      t46 = t41 * t45
      t49 = x1 ** 2
      t50 = t49 * t15
      t55 = log(-0.4D1 * t50 * t11 * x3 * t4)
      t69 = log(-0.4D1 * t11 * t15 * t17)
      t77 = lh ** 2
      t79 = 0.3141592653589793D1 ** 2
      t81 = t69 ** 2
      t87 = -(0.90D2 * t7 * (t8 - t22 * t23 - t25 + t30 * t31) - 0.180D3
     # * t36 * t37) * t41 / 0.5760D4 - t7 * t37 * t46 / 0.32D2 + (0.90D2
     # * t7 * (t25 - t55 * t31) - 0.180D3 * t36 * t31) * t45 / 0.2880D4 
     #+ (-0.180D3 * lh - 0.90D2 * t69) * t7 * t25 / 0.5760D4 + (0.180D3 
     #* t69 * lh + 0.180D3 * t77 - 0.30D2 * t79 + 0.45D2 * t81) * t7 * t
     #31 / 0.5760D4
      t88 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t8
     #7)
      t90 = x3 * x1
      t92 = -0.1D1 + x1
      t94 = t2 * t92 * x3
      t98 = t2 * t92 * t4
      t99 = bbarqqbarH11J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t103 = bbarqqbarH11J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t106 = x1 * z
      t107 = -z - x1 + t106
      t108 = 0.1D1 / t107
      t109 = t92 ** 2
      t114 = log(0.4D1 * t50 / t9 * t17 * t108 * t109)
      t124 = -t7 * t99 * t46 / 0.32D2 + (-0.90D2 * t7 * (t103 - t114 * t
     #99) + 0.180D3 * t36 * t99) * t45 / 0.2880D4
      t125 = FJET(XB1, XB2, s, t2 * t90, -t94, -t2 * x1 * t4, t98, 0.0D0
     #, t124)
      t127 = x3 * z
      t128 = t90 * z
      t129 = x2 * x3
      t130 = t129 * z
      t131 = t129 * x1
      t132 = t129 * t106
      t133 = cos(t13)
      t138 = Sqrt(-x3 * t18 * t107 * x2 * t4)
      t140 = 0.2D1 * t133 * t138
      t146 = x2 * x1
      t147 = t146 * z
      t148 = z + x1 - t106 - x2 * z - t146 + t147 - t127 - t90 + t128 + 
     #t130 + t131 - t132 + t129 + t140
      t152 = t1 ** 2
      t160 = 0.1D1 / (z - t146 + x1 - t106 + t147)
      t162 = bbarqqbarH11J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t167 = FJET(XB1, XB2, s, t2 * x1 * (-t127 - t90 + t128 + t130 + t1
     #31 - t132 - x2 + t129 + t140) * t108, -t94, -t2 * x1 * t148 * t108
     #, t98, s * t152 * x2 * x1 * t92 * t108, -t7 * t107 * t160 * t162 *
     # t41 * t45 / 0.32D2)
      bbarqqbarH1n2e0 = t88 * t87 + t125 * t124 - t167 * t7 * t107 * t16
     #0 * t162 * t46 / 0.32D2

      end function



      doubleprecision function bbarqqbarH1n2em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarqqbarH11J1
      doubleprecision bbarqqbarH11J2
      t2 = s * (-0.1D1 + z)
      t4 = -0.1D1 + x3
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = bbarqqbarH11J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t12 = z ** 2
      t16 = Sin(x4 * 0.3141592653589793D1)
      t17 = t16 ** 2
      t22 = log(-0.4D1 / t12 / z * t17 * x3 * t4)
      t26 = bbarqqbarH11J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t29 = bbarqqbarH11J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t36 = 0.1D1 / x1
      t39 = t7 * t8 / 0.64D2 + (-0.180D3 * lh - 0.90D2 * t22) * t7 * t26
     # / 0.5760D4 - t7 * (t29 - t26) / x2 / 0.64D2 + t7 * t26 * t36 / 0.
     #32D2
      t40 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t3
     #9)
      t44 = -0.1D1 + x1
      t51 = bbarqqbarH11J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t55 = FJET(XB1, XB2, s, t2 * x1 * x3, -t2 * t44 * x3, -t2 * x1 * t
     #4, t2 * t44 * t4, 0.0D0, -t7 * t51 * t36 / 0.32D2)
      bbarqqbarH1n2em1 = t40 * t39 - t55 * t7 * t51 * t36 / 0.32D2

      end function



      doubleprecision function bbarqqbarH1n2em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarqqbarH11J1
      doubleprecision bbarqqbarH11J2
      t2 = s * (-0.1D1 + z)
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = bbarqqbarH11J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t11 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * (-0.1D1 + x3)
     #, 0.0D0, t7 * t8 / 0.64D2)
      bbarqqbarH1n2em2 = t11 * t7 * t8 / 0.64D2

      end function



      doubleprecision function bbarqqbarH1n2em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarqqbarH11J1
      doubleprecision bbarqqbarH11J2
      bbarqqbarH1n2em3 = 0.0D0

      end function



      doubleprecision function bbarqqbarH1n2em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbarqqbarH11J1
      doubleprecision bbarqqbarH11J2
      bbarqqbarH1n2em4 = 0.0D0

      end function
  
 

      doubleprecision function bbarqqbarH11J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = 0.1D1 - z
      t3 = t2 ** 2
      t5 = t1 * t3 * t2
      t6 = x1 ** 2
      t9 = z + x1 * t2
      t10 = t9 ** 2
      t11 = 0.1D1 / t10
      t12 = 0.1D1 - x2
      t13 = x3 * t12
      t15 = 0.1D1 - x3
      t16 = x2 * t15
      t18 = cos(x4 * 0.3141592653589793D1)
      t22 = Sqrt(t13 * t9 * x2 * t15)
      t24 = 0.2D1 * t18 * t22
      t25 = t13 * t9 + t16 - t24
      t27 = 0.1D1 - x1
      t32 = t27 ** 2
      t33 = t5 * t32
      t34 = 0.1D1 / t9
      t35 = x1 * t34
      t39 = x2 * x3
      t43 = t1 * z
      t45 = x2 * x1
      t58 = t1 * t3
      t65 = z ** 2
      t69 = t25 ** 2
      t73 = x3 ** 2
      t78 = t6 * t34
      t85 = t1 * t2
      t94 = t58 * t27
      t97 = z + x1 * t12 * t2
      t98 = t97 * t11
      t103 = t58 * t32
      t104 = t97 * t34
      t122 = -t58 * t78 * t25 - 0.2D1 * t58 * x1 * t27 * x3 + 0.2D1 * t8
     #5 * x1 * z + t58 * t78 * (t15 * t12 * t9 + t39 + t24) - 0.2D1 * t9
     #4 * t98 * x1 * t25 - t103 * t104 * x3 + t103 * t104 * t15 + 0.2D1 
     #* t85 * t27 * t104 * z + 0.2D1 * t94 * t104 * x1 - t33 * t98 * t45
     # + t5 * x2 * t6 * t27 * t34
      bbarqqbarH11J1 = 0.16D2 * wd * (-0.4D1 * t5 * t6 * t11 * t25 * x2 
     #* t27 + 0.2D1 * t33 * t16 * t35 - 0.2D1 * t33 * t39 * t35 + 0.2D1 
     #* t43 * t3 * t45 * t27 * t34 - 0.2D1 * t43 * t2 * t35 * t25 - 0.2D
     #1 * t43 * t2 * t27 * x3 + 0.4D1 * t58 * x1 * t34 * t25 * t27 * x3 
     #+ 0.2D1 * t1 * t65 + 0.2D1 * t58 * t6 * t11 * t69 + 0.2D1 * t58 * 
     #t32 * t73 + t122)

      end function
  
   
 

      doubleprecision function bbarqqbarH11J2
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
      t7 = t4 * t6
      t8 = 0.1D1 - x2
      t11 = z + x1 * t8 * t2
      t13 = z + x1 * t2
      t14 = 0.1D1 / t13
      t15 = t11 * t14
      t18 = x1 ** 2
      t19 = t18 * t14
      t20 = x3 * t8
      t22 = 0.1D1 - x3
      t25 = cos(x4 * 0.3141592653589793D1)
      t29 = Sqrt(t20 * t13 * x2 * t22)
      t31 = 0.2D1 * t25 * t29
      t36 = t1 * t3 * t2
      t48 = t13 ** 2
      t51 = x2 * x1
      bbarqqbarH11J2 = 0.16D2 * wd * (-t7 * t15 * x3 - t4 * t19 * (t20 *
     # t13 + x2 * t22 - t31) + t36 * x2 * t18 * t5 * t14 - 0.2D1 * t4 * 
     #t5 * t15 * x1 - t7 * t15 * t22 + t36 * t6 * t11 / t48 * t51 - t4 *
     # t19 * (t22 * t8 * t13 + x2 * x3 + t31) + 0.2D1 * t1 * z * t3 * t5
     #1 * t5 * t14)

      end function
  
 