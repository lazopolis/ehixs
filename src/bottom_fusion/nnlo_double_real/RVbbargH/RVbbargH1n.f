  
      subroutine RVbbargH1n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision polylog  
      doubleprecision Log  
      doubleprecision RVbbargH1n1e1  
      doubleprecision RVbbargH1n1e0  
      doubleprecision RVbbargH1n1em1  
      doubleprecision RVbbargH1n1em2  
      doubleprecision RVbbargH1n1em3  
      doubleprecision RVbbargH1n1em4  
      doubleprecision RVbbargH1n2e1  
      doubleprecision RVbbargH1n2e0  
      doubleprecision RVbbargH1n2em1  
      doubleprecision RVbbargH1n2em2  
      doubleprecision RVbbargH1n2em3  
      doubleprecision RVbbargH1n2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=RVbbargH1n1e1(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.2)then  
         fff=RVbbargH1n2e1(s, XB1, XB2, z, lh, wd, x1)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=RVbbargH1n1e0(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.2)then  
         fff=RVbbargH1n2e0(s, XB1, XB2, z, lh, wd, x1)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=RVbbargH1n1em1(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.2)then  
         fff=RVbbargH1n2em1(s, XB1, XB2, z, lh, wd, x1)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=RVbbargH1n1em2(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.2)then  
         fff=RVbbargH1n2em2(s, XB1, XB2, z, lh, wd, x1)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=RVbbargH1n1em3(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.2)then  
         fff=RVbbargH1n2em3(s, XB1, XB2, z, lh, wd, x1)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=RVbbargH1n1em4(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.2)then  
         fff=RVbbargH1n2em4(s, XB1, XB2, z, lh, wd, x1)  
      end if  
      end if  
      end subroutine

      doubleprecision function RVbbargH1n1e1
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
      t3 = sqrt(0.3141592653589793D1)
      t4 = t3 * 0.3141592653589793D1
      t5 = t4 * wd
      t6 = z ** 2
      t7 = t6 + 0.1D1
      t8 = log(z)
      t10 = -t1
      t11 = 0.1D1 / t10
      t13 = 0.1D1 - (0.1D1 - z + t8) * t11
      t14 = t7 * t13
      t15 = 0.5772156649015329D0 ** 2
      t19 = polylog(2, t10)
      t21 = -0.2D1 * t8 - 0.2D1 * t19
      t25 = 0.2D1 * z
      t27 = t1 ** 2
      t29 = 0.1D1 / t6 * t27 * t1
      t30 = x1 ** 2
      t32 = log(-t29 * t30)
      t34 = -t6 + t25 - 0.1D1 - t32 * t7
      t36 = t7 * t21 * t11 / 0.2D1 + t34 * t13
      t37 = t5 * t36
      t38 = t4 * 0.5772156649015329D0
      t39 = t4 * lh
      t40 = 0.2D1 * t39
      t42 = (t38 + t40 + t4) * wd
      t43 = t42 * t14
      t49 = polylog(3, t10)
      t51 = (0.1D1 - z - t49) * t11
      t52 = (0.1D1 - z - t19) * t11 - t51
      t57 = -t6 + t25 - 0.1D1
      t59 = t32 ** 2
      t67 = 0.3141592653589793D1 ** 2
      t70 = t67 / 0.12D2 + t15 / 0.2D1
      t71 = t4 * t70
      t72 = lh ** 2
      t73 = t4 * t72
      t78 = (-t71 - 0.2D1 * t73 - t40 - t4 - (t40 + t4) * 0.577215664901
     #5329D0) * wd
      t80 = log(0.2D1)
      t81 = t80 ** 2
      t86 = t7 * wd * 0.3141592653589793D1
      t87 = t8 * x1
      t89 = -0.1D1 + x1
      t90 = 0.1D1 / t89
      t92 = 0.1D1 / t1
      t93 = t3 * t67
      t95 = t3 * t15
      t97 = t3 * 0.5772156649015329D0
      t102 = t93 / 0.4D1 + t95 / 0.2D1 + 0.2D1 * t97 * t80 + 0.2D1 * t3 
     #* t81
      t103 = t92 * t102
      t112 = lh * t6
      t113 = t112 * t8
      t114 = 0.2D1 * t113
      t115 = t8 * z
      t116 = 0.2D1 * t115
      t117 = t6 * t8
      t118 = t6 * t19
      t119 = t8 * t32
      t127 = lh * t8
      t130 = t6 * x1
      t134 = 0.2D1 * t127
      t135 = -t114 + t19 + t116 - t117 + t118 + t87 + t119 * x1 + t117 *
     # x1 - t19 * x1 - t119 - t118 * x1 - 0.2D1 * t115 * x1 - t119 * t6 
     #- t8 + 0.2D1 * t127 * x1 + t119 * t130 + 0.2D1 * t112 * t87 - t134
      t137 = t90 * t92
      t139 = 0.5772156649015329D0 + 0.2D1 * t80
      t140 = t137 * t139
      t143 = 0.1D1 / t3
      t145 = 0.1D1 / x1
      t149 = log(-t29)
      t150 = t8 * t149
      t151 = t150 * t6
      t164 = t15 * 0.5772156649015329D0
      t170 = t81 * t80
      t176 = -t7
      t177 = t176 * t13
      t183 = 0.2D1 * t177 * t39
      t187 = t149 + 0.1D1
      t189 = t6 - t25 + 0.1D1 + t187 * t7
      t192 = (t176 * t21 * t11 / 0.2D1 + t189 * t13) * t4
      t194 = (-t177 * t38 - t183 + t192) * wd
      t206 = -t183 + t192
      t215 = polylog(4, t10)
      t220 = t149 ** 2
      t221 = t220 / 0.2D1
      t222 = -t221 - t149 - 0.1D1
      t232 = t187 * t57 + t222 * t7
      t244 = (t176 * t52 + t189 * t21 * t11 / 0.2D1 + t232 * t13) * t4
      t248 = 0.2D1 * t177 * t73
      t250 = 0.2D1 * t192 * lh
      t264 = (t177 * t71 + t248 - t250 + t244 - t206 * 0.577215664901532
     #9D0) * wd
      t286 = t149 * t6
      t291 = t8 * t220
      t298 = 0.12D2 * t112 * t19 + 0.24D2 * t127 * z - 0.12D2 * t113 + t
     #67 * t6 * t8 - 0.12D2 * t72 * t6 * t8 - 0.6D1 * t151 + 0.6D1 * t28
     #6 * t19 + 0.12D2 * t150 * z - 0.3D1 * t291 * t6 - 0.12D2 * t127 * 
     #t149 + 0.6D1 * t6 * t49
      t316 = -0.12D2 * z * t19 + 0.6D1 * t118 + 0.12D2 * lh * t19 - 0.12
     #D2 * t127 + t67 * t8 - 0.12D2 * t72 * t8 - 0.6D1 * t150 + 0.6D1 * 
     #t149 * t19 - 0.3D1 * t291 + 0.6D1 * t49 + 0.6D1 * t19 - 0.12D2 * t
     #127 * t286
      t325 = -0.3D1 * (-0.2D1 * t5 * t14 * t15 + 0.2D1 * (-t37 + t43) * 
     #0.5772156649015329D0 - t5 * (t7 * t52 + t34 * t21 * t11 / 0.2D1 + 
     #(-t32 * t57 + t59 * t7 / 0.2D1) * t13) + t42 * t36 + t78 * t14 - 0
     #.2D1 * t5 * t14 * t81 + t86 * (-t8 + t87) * t90 * t103 + 0.2D1 * (
     #-0.2D1 * t5 * t14 * 0.5772156649015329D0 - t37 + t43) * t80 - t5 *
     # t135 * t140) * t143 * t145 - 0.3D1 / 0.2D1 * (-0.3141592653589793
     #D1 * wd * (t114 + t134 + t117 - t118 + t8 - t19 - t116 + t151 + t1
     #50) * t92 * t102 + t7 * t8 * 0.3141592653589793D1 * wd * t92 * (0.
     #2804799440705719D1 * t3 + t93 * 0.5772156649015329D0 / 0.4D1 + t93
     # * t80 / 0.2D1 + t3 * t164 / 0.6D1 + t95 * t80 + 0.2D1 * t97 * t81
     # + 0.4D1 / 0.3D1 * t3 * t170) + 0.2D1 * (0.2D1 * t177 * t5 * 0.577
     #2156649015329D0 + t194) * t81 + 0.4D1 / 0.3D1 * t177 * t5 * t170 +
     # 0.2D1 * t194 * t15 + 0.4D1 / 0.3D1 * t177 * t5 * t164 + (t206 * t
     #70 + 0.2D1 * t192 * t72 - 0.4D1 / 0.3D1 * t177 * t4 * t72 * lh + (
     #t189 * t52 + t176 * (-t51 + (0.1D1 - z - t215) * t11) + (t222 * t5
     #7 + (t220 * t149 / 0.6D1 + t221 + t149 + 0.1D1) * t7) * t13 + t232
     # * t21 * t11 / 0.2D1) * t4 - 0.2D1 * t244 * lh - (t248 - t250 + t2
     #44) * 0.5772156649015329D0 + t177 * t4 * (-0.4006856343865313D0 - 
     #t67 * 0.5772156649015329D0 / 0.12D2 - t164 / 0.6D1)) * wd + 0.2D1 
     #* t264 * 0.5772156649015329D0 + 0.2D1 * (0.2D1 * t177 * t5 * t15 +
     # 0.2D1 * t194 * 0.5772156649015329D0 + t264) * t80 - t5 * (t298 + 
     #t316) * t92 * t139 / 0.6D1) * t143
      t326 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t325)
      t330 = t89 * t1
      t332 = log(0.1D1 - t330)
      t336 = 0.1D1 - (t330 + t332) * t90 * t92
      t337 = t176 * t336
      t341 = z * x1
      t343 = log(z + x1 - t341)
      t350 = polylog(2, t330)
      t352 = -0.2D1 * t332 - 0.2D1 * t350
      t356 = 0.2D1 * t341
      t358 = 0.2D1 * z * t30
      t359 = t6 * t30
      t362 = log(t29 * t30 * t89)
      t364 = x1 - t356 + t358 - t30 + 0.1D1 - t359 + t6 - t25 + t130 - t
     #362 * t176
      t366 = t176 * t352 * t137 / 0.2D1 + t364 * t336
      t367 = t5 * t366
      t368 = t42 * t337
      t372 = t343 * t362
      t373 = t6 * t343
      t376 = t343 * z
      t390 = t372 + t373 + t343 * x1 - t6 * t350 - 0.2D1 * t376 * x1 + t
     #343 + t373 * x1 - t373 * t30 - t343 * t30 - 0.2D1 * t376 + 0.2D1 *
     # t376 * t30 + 0.2D1 * t112 * t343 + 0.2D1 * lh * t343 - t350 + t37
     #2 * t6
      t402 = polylog(3, t330)
      t413 = t362 ** 2
      t422 = -0.2D1 * t5 * t337 * t81 + t86 * t343 * t90 * t103 + 0.2D1 
     #* (-0.2D1 * t5 * t337 * 0.5772156649015329D0 - t367 + t368) * t80 
     #- t5 * t390 * t140 - 0.2D1 * t5 * t337 * t15 + 0.2D1 * (-t367 + t3
     #68) * 0.5772156649015329D0 - t5 * (t176 * ((t330 - t350) * t90 * t
     #92 - (t330 - t402) * t90 * t92) + t364 * t352 * t137 / 0.2D1 + (-t
     #362 * (x1 - t30 + 0.1D1 - t356 + t358 - t25 + t130 - t359 + t6) + 
     #t413 * t176 / 0.2D1) * t336) + t42 * t366 + t78 * t337
      t426 = FJET(XB1, XB2, s, -t2 * t89, t2 * x1, 0.0D0, 0.0D0, 0.0D0, 
     #-0.3D1 * t422 * t143 * t145)
      RVbbargH1n1e1 = t326 * t325 - 0.3D1 * t426 * t422 * t143 * t145

      end function



      doubleprecision function RVbbargH1n1e0
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
      t3 = sqrt(0.3141592653589793D1)
      t4 = t3 * 0.3141592653589793D1
      t5 = t4 * wd
      t6 = z ** 2
      t7 = t6 + 0.1D1
      t8 = log(z)
      t10 = -t1
      t11 = 0.1D1 / t10
      t13 = 0.1D1 - (0.1D1 - z + t8) * t11
      t14 = t7 * t13
      t15 = log(0.2D1)
      t22 = polylog(2, t10)
      t24 = -0.2D1 * t8 - 0.2D1 * t22
      t28 = 0.2D1 * z
      t30 = t1 ** 2
      t32 = 0.1D1 / t6 * t30 * t1
      t33 = x1 ** 2
      t35 = log(-t32 * t33)
      t41 = t4 * 0.5772156649015329D0
      t42 = t4 * lh
      t45 = (t41 + 0.2D1 * t42 + t4) * wd
      t47 = t5 * t7
      t50 = -0.1D1 + x1
      t51 = 0.1D1 / t50
      t53 = 0.1D1 / t1
      t55 = 0.5772156649015329D0 + 0.2D1 * t15
      t56 = t53 * t55
      t60 = 0.1D1 / t3
      t62 = 0.1D1 / x1
      t65 = -t7
      t66 = t65 * t13
      t67 = t15 ** 2
      t76 = 0.2D1 * t66 * t42
      t80 = log(-t32)
      t81 = t80 + 0.1D1
      t83 = t6 - t28 + 0.1D1 + t81 * t7
      t86 = (t65 * t24 * t11 / 0.2D1 + t83 * t13) * t4
      t88 = (-t66 * t41 - t76 + t86) * wd
      t92 = 0.5772156649015329D0 ** 2
      t98 = 0.3141592653589793D1 ** 2
      t104 = lh ** 2
      t112 = polylog(3, t10)
      t122 = t80 ** 2
      t158 = t8 * t80
      t167 = -0.3D1 * (-0.2D1 * t5 * t14 * t15 - 0.2D1 * t5 * t14 * 0.57
     #72156649015329D0 - t5 * (t7 * t24 * t11 / 0.2D1 + (-t6 + t28 - 0.1
     #D1 - t35 * t7) * t13) + t45 * t14 + t47 * (-t8 + t8 * x1) * t51 * 
     #t56) * t60 * t62 - 0.3D1 / 0.2D1 * (0.2D1 * t66 * t5 * t67 + 0.2D1
     # * (0.2D1 * t66 * t5 * 0.5772156649015329D0 + t88) * t15 + 0.2D1 *
     # t66 * t5 * t92 + 0.2D1 * t88 * 0.5772156649015329D0 + (t66 * t4 *
     # (t98 / 0.12D2 + t92 / 0.2D1) + 0.2D1 * t66 * t4 * t104 - 0.2D1 * 
     #t86 * lh + (t65 * ((0.1D1 - z - t22) * t11 - (0.1D1 - z - t112) * 
     #t11) + t83 * t24 * t11 / 0.2D1 + (t81 * (-t6 + t28 - 0.1D1) + (-t1
     #22 / 0.2D1 - t80 - 0.1D1) * t7) * t13) * t4 - (-t76 + t86) * 0.577
     #2156649015329D0) * wd + t7 * t8 * 0.3141592653589793D1 * wd * t53 
     #* (t3 * t98 / 0.4D1 + t3 * t92 / 0.2D1 + 0.2D1 * t3 * 0.5772156649
     #015329D0 * t15 + 0.2D1 * t3 * t67) - t5 * (0.2D1 * lh * t6 * t8 + 
     #0.2D1 * lh * t8 + t6 * t8 - t6 * t22 + t8 - t22 - 0.2D1 * t8 * z +
     # t158 * t6 + t158) * t53 * t55) * t60
      t168 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t167)
      t172 = t50 * t1
      t174 = log(0.1D1 - t172)
      t178 = 0.1D1 - (t172 + t174) * t51 * t53
      t179 = t65 * t178
      t186 = polylog(2, t172)
      t193 = z * x1
      t201 = log(t32 * t33 * t50)
      t209 = log(z + x1 - t193)
      t213 = -0.2D1 * t5 * t179 * t15 - 0.2D1 * t5 * t179 * 0.5772156649
     #015329D0 - t5 * (t65 * (-0.2D1 * t174 - 0.2D1 * t186) * t51 * t53 
     #/ 0.2D1 + (x1 - 0.2D1 * t193 + 0.2D1 * z * t33 - t33 + 0.1D1 - t6 
     #* t33 + t6 - t28 + t6 * x1 - t201 * t65) * t178) + t45 * t179 + t4
     #7 * t209 * t51 * t56
      t217 = FJET(XB1, XB2, s, -t2 * t50, t2 * x1, 0.0D0, 0.0D0, 0.0D0, 
     #-0.3D1 * t213 * t60 * t62)
      RVbbargH1n1e0 = t168 * t167 - 0.3D1 * t217 * t213 * t60 * t62

      end function



      doubleprecision function RVbbargH1n1em1
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
      t3 = wd * 0.3141592653589793D1
      t4 = z ** 2
      t5 = t4 + 0.1D1
      t6 = log(z)
      t8 = -t1
      t9 = 0.1D1 / t8
      t11 = 0.1D1 - (0.1D1 - z + t6) * t9
      t13 = 0.1D1 / x1
      t17 = -t5
      t18 = t17 * t11
      t19 = sqrt(0.3141592653589793D1)
      t20 = t19 * 0.3141592653589793D1
      t21 = t20 * wd
      t22 = log(0.2D1)
      t34 = polylog(2, t8)
      t42 = t1 ** 2
      t45 = log(-0.1D1 / t4 * t42 * t1)
      t56 = 0.1D1 / t1
      t66 = 0.3D1 * t3 * t5 * t11 * t13 - 0.3D1 / 0.2D1 * (0.2D1 * t18 *
     # t21 * t22 + 0.2D1 * t18 * t21 * 0.5772156649015329D0 + (-t18 * t2
     #0 * 0.5772156649015329D0 - 0.2D1 * t18 * t20 * lh + (t17 * (-0.2D1
     # * t6 - 0.2D1 * t34) * t9 / 0.2D1 + (t4 - 0.2D1 * z + 0.1D1 + (t45
     # + 0.1D1) * t5) * t11) * t20) * wd + t5 * t6 * t20 * wd * t56 * (0
     #.5772156649015329D0 + 0.2D1 * t22)) / t19
      t67 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t66)
      t69 = -0.1D1 + x1
      t72 = t69 * t1
      t74 = log(0.1D1 - t72)
      t81 = t17 * (0.1D1 - (t72 + t74) / t69 * t56) * t13
      t84 = FJET(XB1, XB2, s, -t2 * t69, t2 * x1, 0.0D0, 0.0D0, 0.0D0, 0
     #.3D1 * t3 * t81)
      RVbbargH1n1em1 = t67 * t66 + 0.3D1 * t84 * wd * 0.3141592653589793
     #D1 * t81

      end function



      doubleprecision function RVbbargH1n1em2
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t1 = -0.1D1 + z
      t3 = z ** 2
      t4 = -0.1D1 - t3
      t5 = log(z)
      t10 = 0.1D1 + (0.1D1 - z + t5) / t1
      t15 = FJET(XB1, XB2, s, s * t1, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -0.3D1
     # / 0.2D1 * t4 * t10 * 0.3141592653589793D1 * wd)
      RVbbargH1n1em2 = -0.3D1 / 0.2D1 * t15 * t4 * t10 * 0.3141592653589
     #793D1 * wd

      end function



      doubleprecision function RVbbargH1n1em3
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      RVbbargH1n1em3 = 0.0D0

      end function



      doubleprecision function RVbbargH1n1em4
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      RVbbargH1n1em4 = 0.0D0

      end function


      doubleprecision function RVbbargH1n2e1
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
      t3 = sqrt(0.3141592653589793D1)
      t4 = t3 * 0.3141592653589793D1
      t5 = t4 * wd
      t6 = z ** 2
      t7 = t6 + 0.1D1
      t8 = log(z)
      t10 = -t1
      t11 = 0.1D1 / t10
      t13 = 0.1D1 - (0.1D1 - z + t8) * t11
      t14 = t7 * t13
      t15 = 0.5772156649015329D0 ** 2
      t19 = polylog(2, t10)
      t21 = -0.2D1 * t8 - 0.2D1 * t19
      t25 = 0.2D1 * z
      t27 = t1 ** 2
      t29 = 0.1D1 / t6 * t27 * t1
      t30 = x1 ** 2
      t32 = log(-t29 * t30)
      t34 = -t6 + t25 - 0.1D1 - t32 * t7
      t36 = t7 * t21 * t11 / 0.2D1 + t34 * t13
      t37 = t5 * t36
      t38 = t4 * 0.5772156649015329D0
      t39 = t4 * lh
      t40 = 0.2D1 * t39
      t42 = (t38 + t40 + t4) * wd
      t43 = t42 * t14
      t49 = polylog(3, t10)
      t51 = (0.1D1 - z - t49) * t11
      t52 = (0.1D1 - z - t19) * t11 - t51
      t57 = -t6 + t25 - 0.1D1
      t59 = t32 ** 2
      t67 = 0.3141592653589793D1 ** 2
      t70 = t67 / 0.12D2 + t15 / 0.2D1
      t71 = t4 * t70
      t72 = lh ** 2
      t73 = t4 * t72
      t78 = (-t71 - 0.2D1 * t73 - t40 - t4 - (t40 + t4) * 0.577215664901
     #5329D0) * wd
      t80 = log(0.2D1)
      t81 = t80 ** 2
      t86 = t7 * wd * 0.3141592653589793D1
      t87 = t8 * x1
      t89 = -0.1D1 + x1
      t90 = 0.1D1 / t89
      t92 = 0.1D1 / t1
      t93 = t3 * t67
      t95 = t3 * t15
      t97 = t3 * 0.5772156649015329D0
      t102 = t93 / 0.4D1 + t95 / 0.2D1 + 0.2D1 * t97 * t80 + 0.2D1 * t3 
     #* t81
      t103 = t92 * t102
      t112 = lh * t6
      t113 = t112 * t8
      t114 = 0.2D1 * t113
      t115 = t8 * z
      t116 = 0.2D1 * t115
      t117 = t6 * t8
      t118 = t6 * t19
      t119 = t8 * t32
      t127 = lh * t8
      t130 = t6 * x1
      t134 = 0.2D1 * t127
      t135 = -t114 + t19 + t116 - t117 + t118 + t87 + t119 * x1 + t117 *
     # x1 - t19 * x1 - t119 - t118 * x1 - 0.2D1 * t115 * x1 - t119 * t6 
     #- t8 + 0.2D1 * t127 * x1 + t119 * t130 + 0.2D1 * t112 * t87 - t134
      t137 = t90 * t92
      t139 = 0.5772156649015329D0 + 0.2D1 * t80
      t140 = t137 * t139
      t143 = 0.1D1 / t3
      t145 = 0.1D1 / x1
      t149 = log(-t29)
      t150 = t8 * t149
      t151 = t150 * t6
      t164 = t15 * 0.5772156649015329D0
      t170 = t81 * t80
      t176 = -t7
      t177 = t176 * t13
      t183 = 0.2D1 * t177 * t39
      t187 = t149 + 0.1D1
      t189 = t6 - t25 + 0.1D1 + t187 * t7
      t192 = (t176 * t21 * t11 / 0.2D1 + t189 * t13) * t4
      t194 = (-t177 * t38 - t183 + t192) * wd
      t206 = -t183 + t192
      t215 = polylog(4, t10)
      t220 = t149 ** 2
      t221 = t220 / 0.2D1
      t222 = -t221 - t149 - 0.1D1
      t232 = t187 * t57 + t222 * t7
      t244 = (t176 * t52 + t189 * t21 * t11 / 0.2D1 + t232 * t13) * t4
      t248 = 0.2D1 * t177 * t73
      t250 = 0.2D1 * t192 * lh
      t264 = (t177 * t71 + t248 - t250 + t244 - t206 * 0.577215664901532
     #9D0) * wd
      t286 = t149 * t6
      t291 = t8 * t220
      t298 = 0.12D2 * t112 * t19 + 0.24D2 * t127 * z - 0.12D2 * t113 + t
     #67 * t6 * t8 - 0.12D2 * t72 * t6 * t8 - 0.6D1 * t151 + 0.6D1 * t28
     #6 * t19 + 0.12D2 * t150 * z - 0.3D1 * t291 * t6 - 0.12D2 * t127 * 
     #t149 + 0.6D1 * t6 * t49
      t316 = -0.12D2 * z * t19 + 0.6D1 * t118 + 0.12D2 * lh * t19 - 0.12
     #D2 * t127 + t67 * t8 - 0.12D2 * t72 * t8 - 0.6D1 * t150 + 0.6D1 * 
     #t149 * t19 - 0.3D1 * t291 + 0.6D1 * t49 + 0.6D1 * t19 - 0.12D2 * t
     #127 * t286
      t325 = -0.3D1 * (-0.2D1 * t5 * t14 * t15 + 0.2D1 * (-t37 + t43) * 
     #0.5772156649015329D0 - t5 * (t7 * t52 + t34 * t21 * t11 / 0.2D1 + 
     #(-t32 * t57 + t59 * t7 / 0.2D1) * t13) + t42 * t36 + t78 * t14 - 0
     #.2D1 * t5 * t14 * t81 + t86 * (-t8 + t87) * t90 * t103 + 0.2D1 * (
     #-0.2D1 * t5 * t14 * 0.5772156649015329D0 - t37 + t43) * t80 - t5 *
     # t135 * t140) * t143 * t145 - 0.3D1 / 0.2D1 * (-0.3141592653589793
     #D1 * wd * (t114 + t134 + t117 - t118 + t8 - t19 - t116 + t151 + t1
     #50) * t92 * t102 + t7 * t8 * 0.3141592653589793D1 * wd * t92 * (0.
     #2804799440705719D1 * t3 + t93 * 0.5772156649015329D0 / 0.4D1 + t93
     # * t80 / 0.2D1 + t3 * t164 / 0.6D1 + t95 * t80 + 0.2D1 * t97 * t81
     # + 0.4D1 / 0.3D1 * t3 * t170) + 0.2D1 * (0.2D1 * t177 * t5 * 0.577
     #2156649015329D0 + t194) * t81 + 0.4D1 / 0.3D1 * t177 * t5 * t170 +
     # 0.2D1 * t194 * t15 + 0.4D1 / 0.3D1 * t177 * t5 * t164 + (t206 * t
     #70 + 0.2D1 * t192 * t72 - 0.4D1 / 0.3D1 * t177 * t4 * t72 * lh + (
     #t189 * t52 + t176 * (-t51 + (0.1D1 - z - t215) * t11) + (t222 * t5
     #7 + (t220 * t149 / 0.6D1 + t221 + t149 + 0.1D1) * t7) * t13 + t232
     # * t21 * t11 / 0.2D1) * t4 - 0.2D1 * t244 * lh - (t248 - t250 + t2
     #44) * 0.5772156649015329D0 + t177 * t4 * (-0.4006856343865313D0 - 
     #t67 * 0.5772156649015329D0 / 0.12D2 - t164 / 0.6D1)) * wd + 0.2D1 
     #* t264 * 0.5772156649015329D0 + 0.2D1 * (0.2D1 * t177 * t5 * t15 +
     # 0.2D1 * t194 * 0.5772156649015329D0 + t264) * t80 - t5 * (t298 + 
     #t316) * t92 * t139 / 0.6D1) * t143
      t326 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t325)
      t330 = t89 * t1
      t332 = log(0.1D1 - t330)
      t336 = 0.1D1 - (t330 + t332) * t90 * t92
      t337 = t176 * t336
      t341 = z * x1
      t343 = log(z + x1 - t341)
      t350 = polylog(2, t330)
      t352 = -0.2D1 * t332 - 0.2D1 * t350
      t356 = 0.2D1 * t341
      t358 = 0.2D1 * z * t30
      t359 = t6 * t30
      t362 = log(t29 * t30 * t89)
      t364 = x1 - t356 + t358 - t30 + 0.1D1 - t359 + t6 - t25 + t130 - t
     #362 * t176
      t366 = t176 * t352 * t137 / 0.2D1 + t364 * t336
      t367 = t5 * t366
      t368 = t42 * t337
      t372 = t343 * t362
      t373 = t6 * t343
      t376 = t343 * z
      t390 = t372 + t373 + t343 * x1 - t6 * t350 - 0.2D1 * t376 * x1 + t
     #343 + t373 * x1 - t373 * t30 - t343 * t30 - 0.2D1 * t376 + 0.2D1 *
     # t376 * t30 + 0.2D1 * t112 * t343 + 0.2D1 * lh * t343 - t350 + t37
     #2 * t6
      t402 = polylog(3, t330)
      t413 = t362 ** 2
      t422 = -0.2D1 * t5 * t337 * t81 + t86 * t343 * t90 * t103 + 0.2D1 
     #* (-0.2D1 * t5 * t337 * 0.5772156649015329D0 - t367 + t368) * t80 
     #- t5 * t390 * t140 - 0.2D1 * t5 * t337 * t15 + 0.2D1 * (-t367 + t3
     #68) * 0.5772156649015329D0 - t5 * (t176 * ((t330 - t350) * t90 * t
     #92 - (t330 - t402) * t90 * t92) + t364 * t352 * t137 / 0.2D1 + (-t
     #362 * (x1 - t30 + 0.1D1 - t356 + t358 - t25 + t130 - t359 + t6) + 
     #t413 * t176 / 0.2D1) * t336) + t42 * t366 + t78 * t337
      t426 = FJET(XB1, XB2, s, t2 * x1, -t2 * t89, 0.0D0, 0.0D0, 0.0D0, 
     #-0.3D1 * t422 * t143 * t145)
      RVbbargH1n2e1 = t326 * t325 - 0.3D1 * t426 * t422 * t143 * t145

      end function



      doubleprecision function RVbbargH1n2e0
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
      t3 = sqrt(0.3141592653589793D1)
      t4 = t3 * 0.3141592653589793D1
      t5 = t4 * wd
      t6 = z ** 2
      t7 = t6 + 0.1D1
      t8 = log(z)
      t10 = -t1
      t11 = 0.1D1 / t10
      t13 = 0.1D1 - (0.1D1 - z + t8) * t11
      t14 = t7 * t13
      t15 = log(0.2D1)
      t22 = polylog(2, t10)
      t24 = -0.2D1 * t8 - 0.2D1 * t22
      t28 = 0.2D1 * z
      t30 = t1 ** 2
      t32 = 0.1D1 / t6 * t30 * t1
      t33 = x1 ** 2
      t35 = log(-t32 * t33)
      t41 = t4 * 0.5772156649015329D0
      t42 = t4 * lh
      t45 = (t41 + 0.2D1 * t42 + t4) * wd
      t47 = t5 * t7
      t50 = -0.1D1 + x1
      t51 = 0.1D1 / t50
      t53 = 0.1D1 / t1
      t55 = 0.5772156649015329D0 + 0.2D1 * t15
      t56 = t53 * t55
      t60 = 0.1D1 / t3
      t62 = 0.1D1 / x1
      t65 = -t7
      t66 = t65 * t13
      t67 = t15 ** 2
      t76 = 0.2D1 * t66 * t42
      t80 = log(-t32)
      t81 = t80 + 0.1D1
      t83 = t6 - t28 + 0.1D1 + t81 * t7
      t86 = (t65 * t24 * t11 / 0.2D1 + t83 * t13) * t4
      t88 = (-t66 * t41 - t76 + t86) * wd
      t92 = 0.5772156649015329D0 ** 2
      t98 = 0.3141592653589793D1 ** 2
      t104 = lh ** 2
      t112 = polylog(3, t10)
      t122 = t80 ** 2
      t158 = t8 * t80
      t167 = -0.3D1 * (-0.2D1 * t5 * t14 * t15 - 0.2D1 * t5 * t14 * 0.57
     #72156649015329D0 - t5 * (t7 * t24 * t11 / 0.2D1 + (-t6 + t28 - 0.1
     #D1 - t35 * t7) * t13) + t45 * t14 + t47 * (-t8 + t8 * x1) * t51 * 
     #t56) * t60 * t62 - 0.3D1 / 0.2D1 * (0.2D1 * t66 * t5 * t67 + 0.2D1
     # * (0.2D1 * t66 * t5 * 0.5772156649015329D0 + t88) * t15 + 0.2D1 *
     # t66 * t5 * t92 + 0.2D1 * t88 * 0.5772156649015329D0 + (t66 * t4 *
     # (t98 / 0.12D2 + t92 / 0.2D1) + 0.2D1 * t66 * t4 * t104 - 0.2D1 * 
     #t86 * lh + (t65 * ((0.1D1 - z - t22) * t11 - (0.1D1 - z - t112) * 
     #t11) + t83 * t24 * t11 / 0.2D1 + (t81 * (-t6 + t28 - 0.1D1) + (-t1
     #22 / 0.2D1 - t80 - 0.1D1) * t7) * t13) * t4 - (-t76 + t86) * 0.577
     #2156649015329D0) * wd + t7 * t8 * 0.3141592653589793D1 * wd * t53 
     #* (t3 * t98 / 0.4D1 + t3 * t92 / 0.2D1 + 0.2D1 * t3 * 0.5772156649
     #015329D0 * t15 + 0.2D1 * t3 * t67) - t5 * (0.2D1 * lh * t6 * t8 + 
     #0.2D1 * lh * t8 + t6 * t8 - t6 * t22 + t8 - t22 - 0.2D1 * t8 * z +
     # t158 * t6 + t158) * t53 * t55) * t60
      t168 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t167)
      t172 = t50 * t1
      t174 = log(0.1D1 - t172)
      t178 = 0.1D1 - (t172 + t174) * t51 * t53
      t179 = t65 * t178
      t186 = polylog(2, t172)
      t193 = z * x1
      t201 = log(t32 * t33 * t50)
      t209 = log(z + x1 - t193)
      t213 = -0.2D1 * t5 * t179 * t15 - 0.2D1 * t5 * t179 * 0.5772156649
     #015329D0 - t5 * (t65 * (-0.2D1 * t174 - 0.2D1 * t186) * t51 * t53 
     #/ 0.2D1 + (x1 - 0.2D1 * t193 + 0.2D1 * z * t33 - t33 + 0.1D1 - t6 
     #* t33 + t6 - t28 + t6 * x1 - t201 * t65) * t178) + t45 * t179 + t4
     #7 * t209 * t51 * t56
      t217 = FJET(XB1, XB2, s, t2 * x1, -t2 * t50, 0.0D0, 0.0D0, 0.0D0, 
     #-0.3D1 * t213 * t60 * t62)
      RVbbargH1n2e0 = t168 * t167 - 0.3D1 * t217 * t213 * t60 * t62

      end function



      doubleprecision function RVbbargH1n2em1
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
      t3 = wd * 0.3141592653589793D1
      t4 = z ** 2
      t5 = t4 + 0.1D1
      t6 = log(z)
      t8 = -t1
      t9 = 0.1D1 / t8
      t11 = 0.1D1 - (0.1D1 - z + t6) * t9
      t13 = 0.1D1 / x1
      t17 = -t5
      t18 = t17 * t11
      t19 = sqrt(0.3141592653589793D1)
      t20 = t19 * 0.3141592653589793D1
      t21 = t20 * wd
      t22 = log(0.2D1)
      t34 = polylog(2, t8)
      t42 = t1 ** 2
      t45 = log(-0.1D1 / t4 * t42 * t1)
      t56 = 0.1D1 / t1
      t66 = 0.3D1 * t3 * t5 * t11 * t13 - 0.3D1 / 0.2D1 * (0.2D1 * t18 *
     # t21 * t22 + 0.2D1 * t18 * t21 * 0.5772156649015329D0 + (-t18 * t2
     #0 * 0.5772156649015329D0 - 0.2D1 * t18 * t20 * lh + (t17 * (-0.2D1
     # * t6 - 0.2D1 * t34) * t9 / 0.2D1 + (t4 - 0.2D1 * z + 0.1D1 + (t45
     # + 0.1D1) * t5) * t11) * t20) * wd + t5 * t6 * t20 * wd * t56 * (0
     #.5772156649015329D0 + 0.2D1 * t22)) / t19
      t67 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t66)
      t70 = -0.1D1 + x1
      t72 = t70 * t1
      t74 = log(0.1D1 - t72)
      t81 = t17 * (0.1D1 - (t72 + t74) / t70 * t56) * t13
      t84 = FJET(XB1, XB2, s, t2 * x1, -t2 * t70, 0.0D0, 0.0D0, 0.0D0, 0
     #.3D1 * t3 * t81)
      RVbbargH1n2em1 = t67 * t66 + 0.3D1 * t84 * wd * 0.3141592653589793
     #D1 * t81

      end function



      doubleprecision function RVbbargH1n2em2
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t1 = -0.1D1 + z
      t3 = z ** 2
      t4 = -0.1D1 - t3
      t5 = log(z)
      t10 = 0.1D1 + (0.1D1 - z + t5) / t1
      t15 = FJET(XB1, XB2, s, 0.0D0, s * t1, 0.0D0, 0.0D0, 0.0D0, -0.3D1
     # / 0.2D1 * t4 * t10 * 0.3141592653589793D1 * wd)
      RVbbargH1n2em2 = -0.3D1 / 0.2D1 * t15 * t4 * t10 * 0.3141592653589
     #793D1 * wd

      end function



      doubleprecision function RVbbargH1n2em3
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      RVbbargH1n2em3 = 0.0D0

      end function



      doubleprecision function RVbbargH1n2em4
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      RVbbargH1n2em4 = 0.0D0

      end function
