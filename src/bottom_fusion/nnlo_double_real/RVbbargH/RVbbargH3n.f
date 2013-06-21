  
      subroutine RVbbargH3n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision polylog  
      doubleprecision Log  
      doubleprecision RVbbargH3n1e1  
      doubleprecision RVbbargH3n1e0  
      doubleprecision RVbbargH3n1em1  
      doubleprecision RVbbargH3n1em2  
      doubleprecision RVbbargH3n1em3  
      doubleprecision RVbbargH3n1em4  
      doubleprecision RVbbargH3n2e1  
      doubleprecision RVbbargH3n2e0  
      doubleprecision RVbbargH3n2em1  
      doubleprecision RVbbargH3n2em2  
      doubleprecision RVbbargH3n2em3  
      doubleprecision RVbbargH3n2em4  
      doubleprecision RVbbargH3n3e1  
      doubleprecision RVbbargH3n3e0  
      doubleprecision RVbbargH3n3em1  
      doubleprecision RVbbargH3n3em2  
      doubleprecision RVbbargH3n3em3  
      doubleprecision RVbbargH3n3em4  
      doubleprecision RVbbargH3n4e1  
      doubleprecision RVbbargH3n4e0  
      doubleprecision RVbbargH3n4em1  
      doubleprecision RVbbargH3n4em2  
      doubleprecision RVbbargH3n4em3  
      doubleprecision RVbbargH3n4em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=RVbbargH3n1e1(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.2)then  
         fff=RVbbargH3n2e1(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.3)then  
         fff=RVbbargH3n3e1(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.4)then  
         fff=RVbbargH3n4e1(s, XB1, XB2, z, lh, wd, x1)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=RVbbargH3n1e0(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.2)then  
         fff=RVbbargH3n2e0(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.3)then  
         fff=RVbbargH3n3e0(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.4)then  
         fff=RVbbargH3n4e0(s, XB1, XB2, z, lh, wd, x1)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=RVbbargH3n1em1(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.2)then  
         fff=RVbbargH3n2em1(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.3)then  
         fff=RVbbargH3n3em1(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.4)then  
         fff=RVbbargH3n4em1(s, XB1, XB2, z, lh, wd, x1)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=RVbbargH3n1em2(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.2)then  
         fff=RVbbargH3n2em2(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.3)then  
         fff=RVbbargH3n3em2(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.4)then  
         fff=RVbbargH3n4em2(s, XB1, XB2, z, lh, wd, x1)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=RVbbargH3n1em3(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.2)then  
         fff=RVbbargH3n2em3(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.3)then  
         fff=RVbbargH3n3em3(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.4)then  
         fff=RVbbargH3n4em3(s, XB1, XB2, z, lh, wd, x1)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=RVbbargH3n1em4(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.2)then  
         fff=RVbbargH3n2em4(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.3)then  
         fff=RVbbargH3n3em4(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.4)then  
         fff=RVbbargH3n4em4(s, XB1, XB2, z, lh, wd, x1)  
      end if  
      end if  
      end subroutine

      doubleprecision function RVbbargH3n1e1
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = lh ** 2
      t7 = 0.3141592653589793D1 ** 2
      t10 = log(z)
      t11 = t10 ** 2
      t20 = t10 * lh
      t26 = -0.480D3 * t3 - 0.5753417909889299D3 + 0.160D3 * t3 * lh - 0
     #.40D2 * t7 * lh + 0.120D3 * t11 * lh - 0.120D3 * t11 + 0.20D2 * t7
     # * t10 + 0.960D3 * lh - 0.240D3 * t10 * t3 + 0.480D3 * t20 - 0.480
     #D3 * t10 - 0.20D2 * t11 * t10 + 0.40D2 * t7
      t28 = 0.1D1 / z
      t29 = log(t28)
      t30 = t29 * 0.3141592653589793D1
      t37 = 0.20D2 * t7 - 0.480D3 - 0.60D2 * t11 - 0.240D3 * t10 - 0.240
     #D3 * t3 + 0.240D3 * t20 + 0.480D3 * lh
      t39 = t29 ** 2
      t40 = t39 * 0.3141592653589793D1
      t43 = -0.240D3 + 0.240D3 * lh - 0.120D3 * t10
      t50 = z ** 2
      t51 = t10 * t50
      t52 = t10 + t51
      t54 = -t1
      t55 = polylog(4, t54)
      t57 = polylog(3, t54)
      t58 = t57 * t50
      t60 = polylog(2, t54)
      t61 = t60 * t50
      t64 = t60 * z
      t72 = 0.3141592653589793D1 * t43
      t75 = 0.3D1 * t61
      t76 = 0.10D2 * z
      t77 = 0.3D1 * t60
      t78 = 0.5D1 * t50
      t79 = 0.2D1 * t51
      t80 = 0.2D1 * t64
      t81 = 0.2D1 * t10
      t82 = t10 * z
      t83 = 0.4D1 * t82
      t84 = -0.5D1 - t58 - t57 + t75 + t76 + t77 - t78 + t79 - t80 + t81
     # - t83
      t86 = 0.3141592653589793D1 * t37
      t90 = 0.5D1 * z
      t91 = 0.3D1 * t10
      t92 = 0.2D1 * t82
      t93 = 0.3D1 * t51
      t94 = -t90 - t60 - t61 + t78 - t91 + t92 - t93
      t99 = x1 ** 2
      t100 = t28 * t99
      t101 = log(t100)
      t109 = 0.3141592653589793D1 * wd
      t111 = t101 ** 2
      t114 = t101 * t94 + 0.5D1 + t58 + t57 - t75 - t76 - t77 + t78 - t7
     #9 + t80 - t81 + t83 - t111 * t52 / 0.2D1
      t118 = 0.1D1 / x1
      t121 = -((0.3141592653589793D1 * t26 - t30 * t37 + t40 * t43 / 0.2
     #D1 + 0.20D2 * t39 * t29 * 0.3141592653589793D1) * t52 - 0.120D3 * 
     #0.3141592653589793D1 * (-t55 * t50 + 0.3D1 * t58 - 0.2D1 * t61 - t
     #55 - 0.2D1 * t60 + 0.4D1 * t64 + 0.3D1 * t57 - 0.2D1 * t57 * z) + 
     #(t72 + 0.120D3 * t30) * t84 + (t86 - t30 * t43 - 0.60D2 * t40) * t
     #94) * wd / 0.720D3 + (t72 * wd * (t90 + t60 + t61 - t78 + t91 - t9
     #2 + t93 + t101 * t52) - t86 * wd * t52 - 0.120D3 * t109 * t114) * 
     #t118 / 0.360D3
      t122 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t121)
      t124 = -0.1D1 + x1
      t127 = t124 * t1
      t129 = log(0.1D1 - t127)
      t130 = t50 * z
      t131 = t129 * t130
      t132 = t131 * x1
      t133 = 0.3D1 * t132
      t134 = t129 * x1
      t135 = t134 * z
      t136 = 0.5D1 * t135
      t137 = t129 * t50
      t138 = t137 * x1
      t139 = 0.5D1 * t138
      t140 = polylog(2, t127)
      t141 = t140 * t130
      t142 = t141 * x1
      t143 = t140 * x1
      t144 = t143 * z
      t145 = t140 * t50
      t146 = t145 * x1
      t147 = x1 * z
      t148 = 0.29D2 * t147
      t149 = x1 * t50
      t150 = 0.29D2 * t149
      t151 = t99 * z
      t152 = 0.83D2 * t151
      t153 = t99 * t50
      t154 = 0.88D2 * t153
      t155 = t99 * x1
      t156 = t155 * z
      t157 = 0.54D2 * t156
      t158 = t155 * t50
      t159 = 0.54D2 * t158
      t160 = t130 * x1
      t161 = 0.8D1 * t160
      t162 = t130 * t99
      t163 = 0.31D2 * t162
      t164 = t133 + t136 - t139 + t142 + t144 - t146 + t148 - t150 - t15
     #2 + t154 + t157 - t159 + t161 - t163
      t165 = t155 * t130
      t166 = 0.18D2 * t165
      t167 = 0.3D1 * t131
      t168 = 0.3D1 * t134
      t169 = t129 * z
      t170 = 0.3D1 * t169
      t171 = t140 * z
      t172 = 0.2D1 * t137
      t174 = log(-t100 * t124)
      t186 = -0.9D1 * t147 + 0.18D2 * t149 - 0.9D1 * t160 - 0.9D1 * t99 
     #- t132 - t135 + t138 + 0.36D2 * t151 - 0.45D2 * t153 + t131 + 0.18
     #D2 * t162 + t134 + t169 + 0.27D2 * t158 - 0.9D1 * t165 - 0.27D2 * 
     #t156 + 0.9D1 * t155
      t188 = 0.8D1 * x1
      t189 = 0.26D2 * t99
      t190 = 0.18D2 * t155
      t191 = 0.5D1 * t130
      t192 = t166 - t167 - t168 - t170 - t141 - t143 - t171 + t172 - t17
     #4 * t186 - t78 - t188 + t189 - t190 + t191
      t196 = 0.1D1 / (-z - x1 + t147)
      t207 = t129 * t99
      t212 = polylog(3, t127)
      t213 = t212 * t130
      t216 = t212 * x1
      t224 = -t90 - t133 - 0.7D1 * t135 + 0.8D1 * t138 - 0.3D1 * t142 - 
     #0.5D1 * t144 + 0.5D1 * t146 + 0.3D1 * t207 * z - 0.3D1 * t207 * t5
     #0 + t213 * x1 + t207 * t130 + t216 * z - t212 * t50 * x1 - 0.4D1 *
     # t147 - t149 + 0.9D1 * t151 - 0.9D1 * t153 + 0.2D1 * t160
      t235 = t133 + t136 - t139 + t142 + t144 - t146 + t148 - t150 - t15
     #2 + t154 + t157 - t159 + t161
      t236 = -t163 + t166 - t167 - t168 - t170 - t141 - t143 - t171 + t1
     #72 - t78 - t188 + t189 - t190 + t191
      t239 = t174 ** 2
      t245 = 0.3D1 * t162 + 0.2D1 * t131 + 0.2D1 * t134 + 0.2D1 * t169 +
     # 0.3D1 * t141 + 0.3D1 * t143 + 0.3D1 * t171 - 0.4D1 * t137 - t207 
     #- t213 - t216 - t212 * z - 0.2D1 * t145 - t174 * (t235 + t236) + t
     #239 * t186 / 0.2D1 + 0.10D2 * t50 + 0.3D1 * x1 - 0.3D1 * t99 - t19
     #1
      t250 = -t72 * wd * (t164 + t192) * t196 - t86 * wd * t186 * t196 +
     # 0.120D3 * t109 * (t224 + t245) * t196
      t253 = FJET(XB1, XB2, s, -t2 * t124, t2 * x1, 0.0D0, 0.0D0, 0.0D0,
     # t250 * t118 / 0.360D3)
      RVbbargH3n1e1 = t122 * t121 + t253 * t250 * t118 / 0.360D3

      end function



      doubleprecision function RVbbargH3n1e0
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.3141592653589793D1 * wd
      t4 = 0.5D1 * z
      t5 = -t1
      t6 = polylog(2, t5)
      t7 = z ** 2
      t8 = t6 * t7
      t9 = 0.5D1 * t7
      t10 = log(z)
      t11 = 0.3D1 * t10
      t12 = t10 * z
      t13 = 0.2D1 * t12
      t14 = t7 * t10
      t15 = 0.3D1 * t14
      t16 = 0.1D1 / z
      t17 = x1 ** 2
      t18 = t16 * t17
      t19 = log(t18)
      t20 = t10 + t14
      t27 = -0.240D3 + 0.240D3 * lh - 0.120D3 * t10
      t28 = 0.3141592653589793D1 * t27
      t33 = 0.1D1 / x1
      t36 = log(t16)
      t37 = t36 * 0.3141592653589793D1
      t42 = 0.3141592653589793D1 ** 2
      t44 = t10 ** 2
      t47 = lh ** 2
      t55 = t36 ** 2
      t60 = polylog(3, t5)
      t70 = -0.5D1 - t60 * t7 - t60 + 0.3D1 * t8 + 0.10D2 * z + 0.3D1 * 
     #t6 - t9 + 0.2D1 * t14 - 0.2D1 * t6 * z + 0.2D1 * t10 - 0.4D1 * t12
      t76 = (-0.120D3 * t3 * (t4 + t6 + t8 - t9 + t11 - t13 + t15 + t19 
     #* t20) - t28 * wd * t20) * t33 / 0.360D3 - ((t28 + 0.120D3 * t37) 
     #* (-t4 - t6 - t8 + t9 - t11 + t13 - t15) + (0.3141592653589793D1 *
     # (0.20D2 * t42 - 0.480D3 - 0.60D2 * t44 - 0.240D3 * t10 - 0.240D3 
     #* t47 + 0.240D3 * t10 * lh + 0.480D3 * lh) - t37 * t27 - 0.60D2 * 
     #t55 * 0.3141592653589793D1) * t20 - 0.120D3 * 0.3141592653589793D1
     # * t70) * wd / 0.720D3
      t77 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t76)
      t79 = -0.1D1 + x1
      t82 = t79 * t1
      t84 = log(0.1D1 - t82)
      t85 = t7 * z
      t86 = t84 * t85
      t87 = t86 * x1
      t89 = t84 * x1
      t90 = t89 * z
      t92 = t84 * t7
      t93 = t92 * x1
      t95 = polylog(2, t82)
      t96 = t95 * t85
      t98 = t95 * x1
      t103 = x1 * z
      t105 = x1 * t7
      t107 = t17 * z
      t109 = t17 * t7
      t111 = t17 * x1
      t112 = t111 * z
      t114 = t111 * t7
      t116 = t85 * x1
      t118 = 0.3D1 * t87 + 0.5D1 * t90 - 0.5D1 * t93 + t96 * x1 + t98 * 
     #z - t95 * t7 * x1 - 0.8D1 * x1 + 0.29D2 * t103 - 0.29D2 * t105 - 0
     #.83D2 * t107 + 0.88D2 * t109 + 0.54D2 * t112 - 0.54D2 * t114 + 0.8
     #D1 * t116
      t119 = t85 * t17
      t121 = t111 * t85
      t125 = t84 * z
      t130 = log(-t18 * t79)
      t142 = -0.9D1 * t103 + 0.18D2 * t105 - 0.9D1 * t116 - 0.9D1 * t17 
     #- t87 - t90 + t93 + 0.36D2 * t107 - 0.45D2 * t109 + t86 + 0.18D2 *
     # t119 + t89 + t125 + 0.27D2 * t114 - 0.9D1 * t121 - 0.27D2 * t112 
     #+ 0.9D1 * t111
      t147 = -0.31D2 * t119 + 0.18D2 * t121 - 0.3D1 * t86 - 0.3D1 * t89 
     #- 0.3D1 * t125 - t96 - t98 - t95 * z + 0.2D1 * t92 - t130 * t142 -
     # t9 + 0.26D2 * t17 - 0.18D2 * t111 + 0.5D1 * t85
      t150 = 0.1D1 / (-z - x1 + t103)
      t157 = 0.120D3 * t3 * (t118 + t147) * t150 - t28 * wd * t142 * t15
     #0
      t160 = FJET(XB1, XB2, s, -t2 * t79, t2 * x1, 0.0D0, 0.0D0, 0.0D0, 
     #t157 * t33 / 0.360D3)
      RVbbargH3n1e0 = t77 * t76 + t160 * t157 * t33 / 0.360D3

      end function



      doubleprecision function RVbbargH3n1em1
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.3141592653589793D1 * wd
      t4 = log(z)
      t5 = z ** 2
      t6 = t4 * t5
      t7 = -t4 - t6
      t8 = 0.1D1 / x1
      t14 = polylog(2, -t1)
      t29 = log(0.1D1 / z)
      t38 = -t3 * t7 * t8 / 0.3D1 - (-0.120D3 * 0.3141592653589793D1 * (
     #-0.5D1 * z - t14 - t14 * t5 + 0.5D1 * t5 - 0.3D1 * t4 + 0.2D1 * t4
     # * z - 0.3D1 * t6) - (0.3141592653589793D1 * (-0.240D3 + 0.240D3 *
     # lh - 0.120D3 * t4) + 0.120D3 * t29 * 0.3141592653589793D1) * t7) 
     #* wd / 0.720D3
      t39 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t38)
      t41 = -0.1D1 + x1
      t44 = x1 * z
      t48 = t5 * z
      t51 = x1 ** 2
      t55 = log(0.1D1 - t41 * t1)
      t56 = t55 * t48
      t58 = t55 * x1
      t69 = t51 * x1
      t77 = -0.9D1 * t44 + 0.18D2 * x1 * t5 - 0.9D1 * t48 * x1 - 0.9D1 *
     # t51 - t56 * x1 - t58 * z + t55 * t5 * x1 + 0.36D2 * t51 * z - 0.4
     #5D2 * t51 * t5 + t56 + 0.18D2 * t48 * t51 + t58 + t55 * z + 0.27D2
     # * t69 * t5 - 0.9D1 * t69 * t48 - 0.27D2 * t69 * z + 0.9D1 * t69
      t81 = t77 / (-z - x1 + t44) * t8
      t84 = FJET(XB1, XB2, s, -t2 * t41, t2 * x1, 0.0D0, 0.0D0, 0.0D0, t
     #3 * t81 / 0.3D1)
      RVbbargH3n1em1 = t39 * t38 + t84 * 0.3141592653589793D1 * wd * t81
     # / 0.3D1

      end function



      doubleprecision function RVbbargH3n1em2
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t3 = log(z)
      t4 = z ** 2
      t6 = t3 + t3 * t4
      t10 = FJET(XB1, XB2, s, s * (-0.1D1 + z), 0.0D0, 0.0D0, 0.0D0, 0.0
     #D0, 0.3141592653589793D1 * t6 * wd / 0.6D1)
      RVbbargH3n1em2 = t10 * 0.3141592653589793D1 * t6 * wd / 0.6D1

      end function



      doubleprecision function RVbbargH3n1em3
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      RVbbargH3n1em3 = 0.0D0

      end function



      doubleprecision function RVbbargH3n1em4
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      RVbbargH3n1em4 = 0.0D0

      end function


      doubleprecision function RVbbargH3n2e1
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = lh ** 2
      t7 = 0.3141592653589793D1 ** 2
      t10 = log(z)
      t11 = t10 ** 2
      t20 = t10 * lh
      t26 = -0.480D3 * t3 - 0.5753417909889299D3 + 0.160D3 * t3 * lh - 0
     #.40D2 * t7 * lh + 0.120D3 * t11 * lh - 0.120D3 * t11 + 0.20D2 * t7
     # * t10 + 0.960D3 * lh - 0.240D3 * t10 * t3 + 0.480D3 * t20 - 0.480
     #D3 * t10 - 0.20D2 * t11 * t10 + 0.40D2 * t7
      t28 = 0.1D1 / z
      t29 = log(t28)
      t30 = t29 * 0.3141592653589793D1
      t37 = 0.20D2 * t7 - 0.480D3 - 0.60D2 * t11 - 0.240D3 * t10 - 0.240
     #D3 * t3 + 0.240D3 * t20 + 0.480D3 * lh
      t39 = t29 ** 2
      t40 = t39 * 0.3141592653589793D1
      t43 = -0.240D3 + 0.240D3 * lh - 0.120D3 * t10
      t50 = z ** 2
      t51 = t10 * t50
      t52 = t10 + t51
      t54 = -t1
      t55 = polylog(4, t54)
      t57 = polylog(3, t54)
      t58 = t57 * t50
      t60 = polylog(2, t54)
      t61 = t60 * t50
      t64 = t60 * z
      t72 = 0.3141592653589793D1 * t43
      t75 = 0.3D1 * t61
      t76 = 0.10D2 * z
      t77 = 0.3D1 * t60
      t78 = 0.5D1 * t50
      t79 = 0.2D1 * t51
      t80 = 0.2D1 * t64
      t81 = 0.2D1 * t10
      t82 = t10 * z
      t83 = 0.4D1 * t82
      t84 = -0.5D1 - t58 - t57 + t75 + t76 + t77 - t78 + t79 - t80 + t81
     # - t83
      t86 = 0.3141592653589793D1 * t37
      t90 = 0.5D1 * z
      t91 = 0.3D1 * t10
      t92 = 0.2D1 * t82
      t93 = 0.3D1 * t51
      t94 = -t90 - t60 - t61 + t78 - t91 + t92 - t93
      t99 = x1 ** 2
      t100 = t28 * t99
      t101 = log(t100)
      t109 = 0.3141592653589793D1 * wd
      t111 = t101 ** 2
      t114 = t101 * t94 + 0.5D1 + t58 + t57 - t75 - t76 - t77 + t78 - t7
     #9 + t80 - t81 + t83 - t111 * t52 / 0.2D1
      t118 = 0.1D1 / x1
      t121 = -((0.3141592653589793D1 * t26 - t30 * t37 + t40 * t43 / 0.2
     #D1 + 0.20D2 * t39 * t29 * 0.3141592653589793D1) * t52 - 0.120D3 * 
     #0.3141592653589793D1 * (-t55 * t50 + 0.3D1 * t58 - 0.2D1 * t61 - t
     #55 - 0.2D1 * t60 + 0.4D1 * t64 + 0.3D1 * t57 - 0.2D1 * t57 * z) + 
     #(t72 + 0.120D3 * t30) * t84 + (t86 - t30 * t43 - 0.60D2 * t40) * t
     #94) * wd / 0.720D3 + (t72 * wd * (t90 + t60 + t61 - t78 + t91 - t9
     #2 + t93 + t101 * t52) - t86 * wd * t52 - 0.120D3 * t109 * t114) * 
     #t118 / 0.360D3
      t122 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t121)
      t125 = -0.1D1 + x1
      t127 = t125 * t1
      t129 = log(0.1D1 - t127)
      t130 = t50 * z
      t131 = t129 * t130
      t132 = t131 * x1
      t133 = 0.3D1 * t132
      t134 = t129 * x1
      t135 = t134 * z
      t136 = 0.5D1 * t135
      t137 = t129 * t50
      t138 = t137 * x1
      t139 = 0.5D1 * t138
      t140 = polylog(2, t127)
      t141 = t140 * t130
      t142 = t141 * x1
      t143 = t140 * x1
      t144 = t143 * z
      t145 = t140 * t50
      t146 = t145 * x1
      t147 = x1 * z
      t148 = 0.29D2 * t147
      t149 = x1 * t50
      t150 = 0.29D2 * t149
      t151 = t99 * z
      t152 = 0.83D2 * t151
      t153 = t99 * t50
      t154 = 0.88D2 * t153
      t155 = t99 * x1
      t156 = t155 * z
      t157 = 0.54D2 * t156
      t158 = t155 * t50
      t159 = 0.54D2 * t158
      t160 = t130 * x1
      t161 = 0.8D1 * t160
      t162 = t130 * t99
      t163 = 0.31D2 * t162
      t164 = t133 + t136 - t139 + t142 + t144 - t146 + t148 - t150 - t15
     #2 + t154 + t157 - t159 + t161 - t163
      t165 = t155 * t130
      t166 = 0.18D2 * t165
      t167 = 0.3D1 * t131
      t168 = 0.3D1 * t134
      t169 = t129 * z
      t170 = 0.3D1 * t169
      t171 = t140 * z
      t172 = 0.2D1 * t137
      t174 = log(-t100 * t125)
      t186 = -0.9D1 * t147 + 0.18D2 * t149 - 0.9D1 * t160 - 0.9D1 * t99 
     #- t132 - t135 + t138 + 0.36D2 * t151 - 0.45D2 * t153 + t131 + 0.18
     #D2 * t162 + t134 + t169 + 0.27D2 * t158 - 0.9D1 * t165 - 0.27D2 * 
     #t156 + 0.9D1 * t155
      t188 = 0.8D1 * x1
      t189 = 0.26D2 * t99
      t190 = 0.18D2 * t155
      t191 = 0.5D1 * t130
      t192 = t166 - t167 - t168 - t170 - t141 - t143 - t171 + t172 - t17
     #4 * t186 - t78 - t188 + t189 - t190 + t191
      t196 = 0.1D1 / (-z - x1 + t147)
      t207 = t129 * t99
      t212 = polylog(3, t127)
      t213 = t212 * t130
      t216 = t212 * x1
      t224 = -t90 - t133 - 0.7D1 * t135 + 0.8D1 * t138 - 0.3D1 * t142 - 
     #0.5D1 * t144 + 0.5D1 * t146 + 0.3D1 * t207 * z - 0.3D1 * t207 * t5
     #0 + t213 * x1 + t207 * t130 + t216 * z - t212 * t50 * x1 - 0.4D1 *
     # t147 - t149 + 0.9D1 * t151 - 0.9D1 * t153 + 0.2D1 * t160
      t235 = t133 + t136 - t139 + t142 + t144 - t146 + t148 - t150 - t15
     #2 + t154 + t157 - t159 + t161
      t236 = -t163 + t166 - t167 - t168 - t170 - t141 - t143 - t171 + t1
     #72 - t78 - t188 + t189 - t190 + t191
      t239 = t174 ** 2
      t245 = 0.3D1 * t162 + 0.2D1 * t131 + 0.2D1 * t134 + 0.2D1 * t169 +
     # 0.3D1 * t141 + 0.3D1 * t143 + 0.3D1 * t171 - 0.4D1 * t137 - t207 
     #- t213 - t216 - t212 * z - 0.2D1 * t145 - t174 * (t235 + t236) + t
     #239 * t186 / 0.2D1 + 0.10D2 * t50 + 0.3D1 * x1 - 0.3D1 * t99 - t19
     #1
      t250 = -t72 * wd * (t164 + t192) * t196 - t86 * wd * t186 * t196 +
     # 0.120D3 * t109 * (t224 + t245) * t196
      t253 = FJET(XB1, XB2, s, t2 * x1, -t2 * t125, 0.0D0, 0.0D0, 0.0D0,
     # t250 * t118 / 0.360D3)
      RVbbargH3n2e1 = t122 * t121 + t253 * t250 * t118 / 0.360D3

      end function



      doubleprecision function RVbbargH3n2e0
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.3141592653589793D1 * wd
      t4 = 0.5D1 * z
      t5 = -t1
      t6 = polylog(2, t5)
      t7 = z ** 2
      t8 = t6 * t7
      t9 = 0.5D1 * t7
      t10 = log(z)
      t11 = 0.3D1 * t10
      t12 = t10 * z
      t13 = 0.2D1 * t12
      t14 = t7 * t10
      t15 = 0.3D1 * t14
      t16 = 0.1D1 / z
      t17 = x1 ** 2
      t18 = t16 * t17
      t19 = log(t18)
      t20 = t10 + t14
      t27 = -0.240D3 + 0.240D3 * lh - 0.120D3 * t10
      t28 = 0.3141592653589793D1 * t27
      t33 = 0.1D1 / x1
      t36 = log(t16)
      t37 = t36 * 0.3141592653589793D1
      t42 = 0.3141592653589793D1 ** 2
      t44 = t10 ** 2
      t47 = lh ** 2
      t55 = t36 ** 2
      t60 = polylog(3, t5)
      t70 = -0.5D1 - t60 * t7 - t60 + 0.3D1 * t8 + 0.10D2 * z + 0.3D1 * 
     #t6 - t9 + 0.2D1 * t14 - 0.2D1 * t6 * z + 0.2D1 * t10 - 0.4D1 * t12
      t76 = (-0.120D3 * t3 * (t4 + t6 + t8 - t9 + t11 - t13 + t15 + t19 
     #* t20) - t28 * wd * t20) * t33 / 0.360D3 - ((t28 + 0.120D3 * t37) 
     #* (-t4 - t6 - t8 + t9 - t11 + t13 - t15) + (0.3141592653589793D1 *
     # (0.20D2 * t42 - 0.480D3 - 0.60D2 * t44 - 0.240D3 * t10 - 0.240D3 
     #* t47 + 0.240D3 * t10 * lh + 0.480D3 * lh) - t37 * t27 - 0.60D2 * 
     #t55 * 0.3141592653589793D1) * t20 - 0.120D3 * 0.3141592653589793D1
     # * t70) * wd / 0.720D3
      t77 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t76)
      t80 = -0.1D1 + x1
      t82 = t80 * t1
      t84 = log(0.1D1 - t82)
      t85 = t7 * z
      t86 = t84 * t85
      t87 = t86 * x1
      t89 = t84 * x1
      t90 = t89 * z
      t92 = t84 * t7
      t93 = t92 * x1
      t95 = polylog(2, t82)
      t96 = t95 * t85
      t98 = t95 * x1
      t103 = x1 * z
      t105 = x1 * t7
      t107 = t17 * z
      t109 = t17 * t7
      t111 = t17 * x1
      t112 = t111 * z
      t114 = t111 * t7
      t116 = t85 * x1
      t118 = 0.3D1 * t87 + 0.5D1 * t90 - 0.5D1 * t93 + t96 * x1 + t98 * 
     #z - t95 * t7 * x1 - 0.8D1 * x1 + 0.29D2 * t103 - 0.29D2 * t105 - 0
     #.83D2 * t107 + 0.88D2 * t109 + 0.54D2 * t112 - 0.54D2 * t114 + 0.8
     #D1 * t116
      t119 = t85 * t17
      t121 = t111 * t85
      t125 = t84 * z
      t130 = log(-t18 * t80)
      t142 = -0.9D1 * t103 + 0.18D2 * t105 - 0.9D1 * t116 - 0.9D1 * t17 
     #- t87 - t90 + t93 + 0.36D2 * t107 - 0.45D2 * t109 + t86 + 0.18D2 *
     # t119 + t89 + t125 + 0.27D2 * t114 - 0.9D1 * t121 - 0.27D2 * t112 
     #+ 0.9D1 * t111
      t147 = -0.31D2 * t119 + 0.18D2 * t121 - 0.3D1 * t86 - 0.3D1 * t89 
     #- 0.3D1 * t125 - t96 - t98 - t95 * z + 0.2D1 * t92 - t130 * t142 -
     # t9 + 0.26D2 * t17 - 0.18D2 * t111 + 0.5D1 * t85
      t150 = 0.1D1 / (-z - x1 + t103)
      t157 = 0.120D3 * t3 * (t118 + t147) * t150 - t28 * wd * t142 * t15
     #0
      t160 = FJET(XB1, XB2, s, t2 * x1, -t2 * t80, 0.0D0, 0.0D0, 0.0D0, 
     #t157 * t33 / 0.360D3)
      RVbbargH3n2e0 = t77 * t76 + t160 * t157 * t33 / 0.360D3

      end function



      doubleprecision function RVbbargH3n2em1
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.3141592653589793D1 * wd
      t4 = log(z)
      t5 = z ** 2
      t6 = t4 * t5
      t7 = -t4 - t6
      t8 = 0.1D1 / x1
      t14 = polylog(2, -t1)
      t29 = log(0.1D1 / z)
      t38 = -t3 * t7 * t8 / 0.3D1 - (-0.120D3 * 0.3141592653589793D1 * (
     #-0.5D1 * z - t14 - t14 * t5 + 0.5D1 * t5 - 0.3D1 * t4 + 0.2D1 * t4
     # * z - 0.3D1 * t6) - (0.3141592653589793D1 * (-0.240D3 + 0.240D3 *
     # lh - 0.120D3 * t4) + 0.120D3 * t29 * 0.3141592653589793D1) * t7) 
     #* wd / 0.720D3
      t39 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t38)
      t42 = -0.1D1 + x1
      t44 = x1 * z
      t48 = t5 * z
      t51 = x1 ** 2
      t55 = log(0.1D1 - t42 * t1)
      t56 = t55 * t48
      t58 = t55 * x1
      t69 = t51 * x1
      t77 = -0.9D1 * t44 + 0.18D2 * x1 * t5 - 0.9D1 * t48 * x1 - 0.9D1 *
     # t51 - t56 * x1 - t58 * z + t55 * t5 * x1 + 0.36D2 * t51 * z - 0.4
     #5D2 * t51 * t5 + t56 + 0.18D2 * t48 * t51 + t58 + t55 * z + 0.27D2
     # * t69 * t5 - 0.9D1 * t69 * t48 - 0.27D2 * t69 * z + 0.9D1 * t69
      t81 = t77 / (-z - x1 + t44) * t8
      t84 = FJET(XB1, XB2, s, t2 * x1, -t2 * t42, 0.0D0, 0.0D0, 0.0D0, t
     #3 * t81 / 0.3D1)
      RVbbargH3n2em1 = t39 * t38 + t84 * 0.3141592653589793D1 * wd * t81
     # / 0.3D1

      end function



      doubleprecision function RVbbargH3n2em2
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t3 = log(z)
      t4 = z ** 2
      t6 = t3 + t3 * t4
      t10 = FJET(XB1, XB2, s, 0.0D0, s * (-0.1D1 + z), 0.0D0, 0.0D0, 0.0
     #D0, 0.3141592653589793D1 * t6 * wd / 0.6D1)
      RVbbargH3n2em2 = t10 * 0.3141592653589793D1 * t6 * wd / 0.6D1

      end function



      doubleprecision function RVbbargH3n2em3
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      RVbbargH3n2em3 = 0.0D0

      end function



      doubleprecision function RVbbargH3n2em4
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      RVbbargH3n2em4 = 0.0D0

      end function


      doubleprecision function RVbbargH3n3e1
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = -0.1D1 + x1
      t6 = 0.3141592653589793D1 * wd
      t9 = t3 ** 2
      t11 = log(0.1D1 / z * x1 * t9)
      t15 = log(z)
      t18 = 0.3141592653589793D1 * (-0.240D3 + 0.240D3 * lh - 0.120D3 * 
     #t15)
      t21 = x1 ** 2
      t23 = x1 * z
      t25 = z ** 2
      t26 = x1 * t25
      t29 = t1 * x1
      t31 = log(0.1D1 + t29)
      t33 = polylog(2, -t29)
      t34 = t31 * x1
      t35 = t34 * z
      t37 = t33 * x1
      t38 = t37 * z
      t39 = t34 * t25
      t41 = t37 * t25
      t42 = t25 * z
      t44 = t31 * t42 * x1
      t47 = t33 * t42 * x1
      t48 = -0.28D2 * t21 - 0.25D2 * t23 + 0.15D2 * t26 + 0.10D2 * x1 - 
     #0.3D1 * t31 - t33 - 0.5D1 * t35 - t38 + 0.5D1 * t39 + t41 - 0.3D1 
     #* t44 - t47
      t50 = t21 * z
      t52 = t21 * t25
      t54 = t42 * t21
      t57 = 0.2D1 * t31 * z
      t58 = t31 * t25
      t60 = t33 * t25
      t61 = t21 * x1
      t62 = t61 * t25
      t64 = t61 * t42
      t66 = t61 * z
      t69 = 0.3D1 * t34 + t37 + 0.79D2 * t50 - 0.74D2 * t52 + 0.23D2 * t
     #54 + t57 - 0.3D1 * t58 - t60 + 0.54D2 * t62 - 0.18D2 * t64 - 0.54D
     #2 * t66 + 0.18D2 * t61
      t72 = t11 ** 2
      t77 = 0.3141592653589793D1 ** 2
      t79 = t15 ** 2
      t82 = lh ** 2
      t102 = 0.18D2 * t21 + 0.18D2 * t23 - 0.9D1 * t26 - 0.9D1 * x1 + t3
     #1 + t35 - t39 + t44 - t34 - 0.45D2 * t50 + 0.36D2 * t52 - 0.9D1 * 
     #t54 + t58 - 0.27D2 * t62 + 0.9D1 * t64 + 0.27D2 * t66 - 0.9D1 * t6
     #1
      t111 = polylog(3, -t29)
      t112 = t111 * x1
      t113 = t31 * t21
      t119 = t35 + 0.5D1 * t38 - 0.2D1 * t39 - 0.5D1 * t41 + t44 + 0.3D1
     # * t47 + 0.3D1 * t33 - 0.8D1 * t42 * x1 + t112 - t113 - 0.2D1 * t3
     #3 * z - t111 * t25 + 0.3D1 * x1 - 0.3D1 * t21 + t31
      t136 = -t111 - 0.14D2 * t23 + 0.19D2 * t26 - 0.3D1 * t37 + 0.9D1 *
     # t50 - 0.9D1 * t52 + 0.3D1 * t54 - t57 - t111 * t42 * x1 + t113 * 
     #t42 + t58 + 0.3D1 * t60 - t112 * z + t112 * t25 + 0.3D1 * t113 * z
     # - 0.3D1 * t113 * t25
      t140 = (0.120D3 * t6 * t11 + t18 * wd) * (t48 + t69) + (-0.60D2 * 
     #t6 * t72 - t18 * wd * t11 + 0.3141592653589793D1 * (0.20D2 * t77 -
     # 0.480D3 - 0.60D2 * t79 - 0.240D3 * t15 - 0.240D3 * t82 + 0.240D3 
     #* t15 * lh + 0.480D3 * lh) * wd) * t102 - 0.120D3 * t6 * (t119 + t
     #136)
      t142 = 0.1D1 / (-x1 + t23 + 0.1D1)
      t144 = 0.1D1 / x1
      t147 = FJET(XB1, XB2, s, -t2 * t3, t2 * x1, 0.0D0, 0.0D0, 0.0D0, t
     #140 * t142 * t144 / 0.360D3)
      RVbbargH3n3e1 = t147 * t140 * t142 * t144 / 0.360D3

      end function



      doubleprecision function RVbbargH3n3e0
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = -0.1D1 + x1
      t6 = 0.3141592653589793D1 * wd
      t7 = x1 ** 2
      t9 = x1 * z
      t11 = z ** 2
      t12 = x1 * t11
      t15 = t1 * x1
      t17 = log(0.1D1 + t15)
      t19 = polylog(2, -t15)
      t20 = t17 * x1
      t21 = t20 * z
      t23 = t19 * x1
      t25 = t20 * t11
      t28 = t11 * z
      t30 = t17 * t28 * x1
      t34 = -0.28D2 * t7 - 0.25D2 * t9 + 0.15D2 * t12 + 0.10D2 * x1 - 0.
     #3D1 * t17 - t19 - 0.5D1 * t21 - t23 * z + 0.5D1 * t25 + t23 * t11 
     #- 0.3D1 * t30 - t19 * t28 * x1
      t36 = t7 * z
      t38 = t7 * t11
      t40 = t28 * t7
      t44 = t17 * t11
      t47 = t7 * x1
      t48 = t47 * t11
      t50 = t47 * t28
      t52 = t47 * z
      t55 = 0.3D1 * t20 + t23 + 0.79D2 * t36 - 0.74D2 * t38 + 0.23D2 * t
     #40 + 0.2D1 * t17 * z - 0.3D1 * t44 - t19 * t11 + 0.54D2 * t48 - 0.
     #18D2 * t50 - 0.54D2 * t52 + 0.18D2 * t47
      t61 = t3 ** 2
      t63 = log(0.1D1 / z * x1 * t61)
      t67 = log(z)
      t84 = 0.18D2 * t7 + 0.18D2 * t9 - 0.9D1 * t12 - 0.9D1 * x1 + t17 +
     # t21 - t25 + t30 - t20 - 0.45D2 * t36 + 0.36D2 * t38 - 0.9D1 * t40
     # + t44 - 0.27D2 * t48 + 0.9D1 * t50 + 0.27D2 * t52 - 0.9D1 * t47
      t86 = -0.120D3 * t6 * (t34 + t55) + (0.120D3 * t6 * t63 + 0.314159
     #2653589793D1 * (-0.240D3 + 0.240D3 * lh - 0.120D3 * t67) * wd) * t
     #84
      t88 = 0.1D1 / (-x1 + t9 + 0.1D1)
      t90 = 0.1D1 / x1
      t93 = FJET(XB1, XB2, s, -t2 * t3, t2 * x1, 0.0D0, 0.0D0, 0.0D0, t8
     #6 * t88 * t90 / 0.360D3)
      RVbbargH3n3e0 = t93 * t86 * t88 * t90 / 0.360D3

      end function



      doubleprecision function RVbbargH3n3em1
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t7 = x1 ** 2
      t9 = x1 * z
      t11 = z ** 2
      t17 = log(0.1D1 + t1 * x1)
      t18 = t17 * x1
      t21 = t11 * z
      t31 = t7 * x1
      t39 = 0.18D2 * t7 + 0.18D2 * t9 - 0.9D1 * x1 * t11 - 0.9D1 * x1 + 
     #t17 + t18 * z - t18 * t11 + t17 * t21 * x1 - t18 - 0.45D2 * t7 * z
     # + 0.36D2 * t7 * t11 - 0.9D1 * t21 * t7 + t17 * t11 - 0.27D2 * t31
     # * t11 + 0.9D1 * t31 * t21 + 0.27D2 * t31 * z - 0.9D1 * t31
      t44 = t39 / (-x1 + t9 + 0.1D1) / x1
      t47 = FJET(XB1, XB2, s, -t2 * (-0.1D1 + x1), t2 * x1, 0.0D0, 0.0D0
     #, 0.0D0, -0.3141592653589793D1 * wd * t44 / 0.3D1)
      RVbbargH3n3em1 = -t47 * 0.3141592653589793D1 * wd * t44 / 0.3D1

      end function



      doubleprecision function RVbbargH3n3em2
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      RVbbargH3n3em2 = 0.0D0

      end function



      doubleprecision function RVbbargH3n3em3
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      RVbbargH3n3em3 = 0.0D0

      end function



      doubleprecision function RVbbargH3n3em4
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      RVbbargH3n3em4 = 0.0D0

      end function


      doubleprecision function RVbbargH3n4e1
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x1
      t6 = 0.3141592653589793D1 * wd
      t9 = t4 ** 2
      t11 = log(0.1D1 / z * x1 * t9)
      t15 = log(z)
      t18 = 0.3141592653589793D1 * (-0.240D3 + 0.240D3 * lh - 0.120D3 * 
     #t15)
      t21 = x1 ** 2
      t23 = x1 * z
      t25 = z ** 2
      t26 = x1 * t25
      t29 = t1 * x1
      t31 = log(0.1D1 + t29)
      t33 = polylog(2, -t29)
      t34 = t31 * x1
      t35 = t34 * z
      t37 = t33 * x1
      t38 = t37 * z
      t39 = t34 * t25
      t41 = t37 * t25
      t42 = t25 * z
      t44 = t31 * t42 * x1
      t47 = t33 * t42 * x1
      t48 = -0.28D2 * t21 - 0.25D2 * t23 + 0.15D2 * t26 + 0.10D2 * x1 - 
     #0.3D1 * t31 - t33 - 0.5D1 * t35 - t38 + 0.5D1 * t39 + t41 - 0.3D1 
     #* t44 - t47
      t50 = t21 * z
      t52 = t21 * t25
      t54 = t42 * t21
      t57 = 0.2D1 * t31 * z
      t58 = t31 * t25
      t60 = t33 * t25
      t61 = t21 * x1
      t62 = t61 * t25
      t64 = t61 * t42
      t66 = t61 * z
      t69 = 0.3D1 * t34 + t37 + 0.79D2 * t50 - 0.74D2 * t52 + 0.23D2 * t
     #54 + t57 - 0.3D1 * t58 - t60 + 0.54D2 * t62 - 0.18D2 * t64 - 0.54D
     #2 * t66 + 0.18D2 * t61
      t72 = t11 ** 2
      t77 = 0.3141592653589793D1 ** 2
      t79 = t15 ** 2
      t82 = lh ** 2
      t102 = 0.18D2 * t21 + 0.18D2 * t23 - 0.9D1 * t26 - 0.9D1 * x1 + t3
     #1 + t35 - t39 + t44 - t34 - 0.45D2 * t50 + 0.36D2 * t52 - 0.9D1 * 
     #t54 + t58 - 0.27D2 * t62 + 0.9D1 * t64 + 0.27D2 * t66 - 0.9D1 * t6
     #1
      t111 = polylog(3, -t29)
      t112 = t111 * x1
      t113 = t31 * t21
      t119 = -0.3D1 * t37 + 0.9D1 * t50 - 0.9D1 * t52 + 0.3D1 * t54 - t5
     #7 + t58 + 0.3D1 * t60 - 0.8D1 * t42 * x1 + t112 - t113 - 0.2D1 * t
     #33 * z - t111 * t25 + t35 + 0.5D1 * t38 - 0.2D1 * t39
      t136 = -0.5D1 * t41 + t44 + 0.3D1 * t47 - t111 - t112 * z + t112 *
     # t25 + 0.3D1 * t113 * z - 0.3D1 * t113 * t25 - t111 * t42 * x1 + t
     #113 * t42 - 0.14D2 * t23 + 0.19D2 * t26 + 0.3D1 * x1 - 0.3D1 * t21
     # + t31 + 0.3D1 * t33
      t140 = (0.120D3 * t6 * t11 + t18 * wd) * (t48 + t69) + (-0.60D2 * 
     #t6 * t72 - t18 * wd * t11 + 0.3141592653589793D1 * (0.20D2 * t77 -
     # 0.480D3 - 0.60D2 * t79 - 0.240D3 * t15 - 0.240D3 * t82 + 0.240D3 
     #* t15 * lh + 0.480D3 * lh) * wd) * t102 - 0.120D3 * t6 * (t119 + t
     #136)
      t142 = 0.1D1 / (-x1 + t23 + 0.1D1)
      t144 = 0.1D1 / x1
      t147 = FJET(XB1, XB2, s, t2 * x1, -t2 * t4, 0.0D0, 0.0D0, 0.0D0, t
     #140 * t142 * t144 / 0.360D3)
      RVbbargH3n4e1 = t147 * t140 * t142 * t144 / 0.360D3

      end function



      doubleprecision function RVbbargH3n4e0
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x1
      t6 = 0.3141592653589793D1 * wd
      t7 = x1 ** 2
      t9 = x1 * z
      t11 = z ** 2
      t12 = x1 * t11
      t15 = t1 * x1
      t17 = log(0.1D1 + t15)
      t19 = polylog(2, -t15)
      t20 = t17 * x1
      t21 = t20 * z
      t23 = t19 * x1
      t25 = t20 * t11
      t28 = t11 * z
      t30 = t17 * t28 * x1
      t34 = -0.28D2 * t7 - 0.25D2 * t9 + 0.15D2 * t12 + 0.10D2 * x1 - 0.
     #3D1 * t17 - t19 - 0.5D1 * t21 - t23 * z + 0.5D1 * t25 + t23 * t11 
     #- 0.3D1 * t30 - t19 * t28 * x1
      t36 = t7 * z
      t38 = t7 * t11
      t40 = t28 * t7
      t44 = t17 * t11
      t47 = t7 * x1
      t48 = t47 * t11
      t50 = t47 * t28
      t52 = t47 * z
      t55 = 0.3D1 * t20 + t23 + 0.79D2 * t36 - 0.74D2 * t38 + 0.23D2 * t
     #40 + 0.2D1 * t17 * z - 0.3D1 * t44 - t19 * t11 + 0.54D2 * t48 - 0.
     #18D2 * t50 - 0.54D2 * t52 + 0.18D2 * t47
      t61 = t4 ** 2
      t63 = log(0.1D1 / z * x1 * t61)
      t67 = log(z)
      t84 = 0.18D2 * t7 + 0.18D2 * t9 - 0.9D1 * t12 - 0.9D1 * x1 + t17 +
     # t21 - t25 + t30 - t20 - 0.45D2 * t36 + 0.36D2 * t38 - 0.9D1 * t40
     # + t44 - 0.27D2 * t48 + 0.9D1 * t50 + 0.27D2 * t52 - 0.9D1 * t47
      t86 = -0.120D3 * t6 * (t34 + t55) + (0.120D3 * t6 * t63 + 0.314159
     #2653589793D1 * (-0.240D3 + 0.240D3 * lh - 0.120D3 * t67) * wd) * t
     #84
      t88 = 0.1D1 / (-x1 + t9 + 0.1D1)
      t90 = 0.1D1 / x1
      t93 = FJET(XB1, XB2, s, t2 * x1, -t2 * t4, 0.0D0, 0.0D0, 0.0D0, t8
     #6 * t88 * t90 / 0.360D3)
      RVbbargH3n4e0 = t93 * t86 * t88 * t90 / 0.360D3

      end function



      doubleprecision function RVbbargH3n4em1
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t7 = x1 ** 2
      t9 = x1 * z
      t11 = z ** 2
      t17 = log(0.1D1 + t1 * x1)
      t18 = t17 * x1
      t21 = t11 * z
      t31 = t7 * x1
      t39 = 0.18D2 * t7 + 0.18D2 * t9 - 0.9D1 * x1 * t11 - 0.9D1 * x1 + 
     #t17 + t18 * z - t18 * t11 + t17 * t21 * x1 - t18 - 0.45D2 * t7 * z
     # + 0.36D2 * t7 * t11 - 0.9D1 * t21 * t7 + t17 * t11 - 0.27D2 * t31
     # * t11 + 0.9D1 * t31 * t21 + 0.27D2 * t31 * z - 0.9D1 * t31
      t44 = t39 / (-x1 + t9 + 0.1D1) / x1
      t47 = FJET(XB1, XB2, s, t2 * x1, -t2 * (-0.1D1 + x1), 0.0D0, 0.0D0
     #, 0.0D0, -0.3141592653589793D1 * wd * t44 / 0.3D1)
      RVbbargH3n4em1 = -t47 * 0.3141592653589793D1 * wd * t44 / 0.3D1

      end function



      doubleprecision function RVbbargH3n4em2
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      RVbbargH3n4em2 = 0.0D0

      end function



      doubleprecision function RVbbargH3n4em3
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      RVbbargH3n4em3 = 0.0D0

      end function



      doubleprecision function RVbbargH3n4em4
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      RVbbargH3n4em4 = 0.0D0

      end function
