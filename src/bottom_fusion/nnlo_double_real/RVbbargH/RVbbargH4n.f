  
      subroutine RVbbargH4n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision polylog  
      doubleprecision Log  
      doubleprecision RVbbargH4n1e1  
      doubleprecision RVbbargH4n1e0  
      doubleprecision RVbbargH4n1em1  
      doubleprecision RVbbargH4n1em2  
      doubleprecision RVbbargH4n1em3  
      doubleprecision RVbbargH4n1em4  
      doubleprecision RVbbargH4n2e1  
      doubleprecision RVbbargH4n2e0  
      doubleprecision RVbbargH4n2em1  
      doubleprecision RVbbargH4n2em2  
      doubleprecision RVbbargH4n2em3  
      doubleprecision RVbbargH4n2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=RVbbargH4n1e1(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.2)then  
         fff=RVbbargH4n2e1(s, XB1, XB2, z, lh, wd, x1)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=RVbbargH4n1e0(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.2)then  
         fff=RVbbargH4n2e0(s, XB1, XB2, z, lh, wd, x1)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=RVbbargH4n1em1(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.2)then  
         fff=RVbbargH4n2em1(s, XB1, XB2, z, lh, wd, x1)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=RVbbargH4n1em2(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.2)then  
         fff=RVbbargH4n2em2(s, XB1, XB2, z, lh, wd, x1)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=RVbbargH4n1em3(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.2)then  
         fff=RVbbargH4n2em3(s, XB1, XB2, z, lh, wd, x1)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=RVbbargH4n1em4(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.2)then  
         fff=RVbbargH4n2em4(s, XB1, XB2, z, lh, wd, x1)  
      end if  
      end if  
      end subroutine

      doubleprecision function RVbbargH4n1e1
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t2 = s * (-0.1D1 + z)
      t3 = 0.3141592653589793D1 ** 2
      t4 = t3 * 0.3141592653589793D1
      t5 = 0.120D3 * t4
      t6 = lh ** 2
      t7 = 0.240D3 * t6
      t8 = log(z)
      t9 = t8 ** 2
      t10 = 0.60D2 * t9
      t11 = t8 * lh
      t12 = 0.240D3 * t11
      t16 = -0.480D3 - t7 - t10 + t12 + 0.20D2 * t3 - 0.240D3 * t8 + 0.4
     #80D3 * lh
      t17 = 0.3141592653589793D1 * t16
      t18 = 0.2D1 * t17
      t19 = t9 * t8
      t21 = t9 * lh
      t23 = t6 * lh
      t26 = t3 * lh
      t30 = t8 * t6
      t34 = t3 * t8
      t37 = -0.5753417909889299D3 - 0.20D2 * t19 + 0.120D3 * t21 + 0.160
     #D3 * t23 - 0.120D3 * t9 - 0.40D2 * t26 - 0.480D3 * t6 + 0.480D3 * 
     #t11 - 0.240D3 * t30 - 0.480D3 * t8 + 0.960D3 * lh + 0.20D2 * t34 +
     # 0.40D2 * t3
      t38 = 0.3141592653589793D1 * t37
      t39 = 0.120D3 * t8
      t40 = 0.240D3 * lh
      t41 = -t39 + t40 - 0.240D3
      t48 = (-t5 - t18 + t38 - t4 * t41 / 0.2D1 - (t39 - t40) * t4 / 0.6
     #D1) * wd
      t49 = z ** 2
      t50 = 0.1D1 + t49
      t52 = x1 ** 2
      t53 = 0.1D1 / z
      t55 = log(t52 * t53)
      t56 = t55 ** 2
      t57 = 0.2D1 * z
      t58 = -0.1D1 - t49 + t57
      t69 = 0.3141592653589793D1 * t41
      t70 = 0.240D3 * 0.3141592653589793D1 + t69
      t79 = -0.2D1 * t69 + 0.40D2 * t4 + t17
      t85 = 0.1D1 / x1
      t88 = log(t53)
      t89 = t88 * 0.3141592653589793D1
      t90 = t89 * t41
      t92 = t88 ** 2
      t93 = t92 * 0.3141592653589793D1
      t95 = t89 * t16
      t96 = t93 * t41
      t99 = t92 * t88 * 0.3141592653589793D1
      t103 = (t69 + 0.120D3 * t89) * t3
      t110 = 0.1D1 / 0.3141592653589793D1
      t116 = t3 ** 2
      t117 = t116 * 0.3141592653589793D1
      t125 = t9 ** 2
      t129 = t6 ** 2
      t132 = 0.1150683581977860D4 * lh + 0.80D2 * t3 - 0.960D3 * t6 - 0.
     #5753417909889299D3 * t8 - 0.240D3 * t9 - 0.40D2 * t19 - 0.5D1 * t1
     #25 + 0.3D1 * t116 + 0.320D3 * t23 - 0.80D2 * t129 + 0.40D2 * t34
      t149 = -0.1150683581977860D4 + 0.40D2 * t3 * t6 + 0.10D2 * t3 * t9
     # + 0.960D3 * t11 + 0.160D3 * t8 * t23 - 0.120D3 * t9 * t6 + 0.40D2
     # * t19 * lh + 0.240D3 * t21 - 0.480D3 * t30 - 0.80D2 * t26 - 0.40D
     #2 * t34 * lh
      t157 = t92 ** 2
      t173 = -0.2D1 * t38 + 0.2D1 * t95 - t96 - 0.40D2 * t99 + t103 - 0.
     #4D1 * t117 + 0.3141592653589793D1 * (t132 + t149) - t89 * t37 + t9
     #3 * t16 / 0.2D1 - t99 * t41 / 0.6D1 - 0.5D1 * t157 * 0.31415926535
     #89793D1 - (t17 - t90 - 0.60D2 * t93) * t3 / 0.2D1 - (-0.60D2 * t3 
     #+ t7 + t10 - t12 - 0.120D3 * t88 * t8 + 0.240D3 * t88 * lh + 0.60D
     #2 * t92) * t4 / 0.6D1
      t178 = -t50
      t182 = 0.5772156649015329D0 ** 2
      t185 = t182 ** 2
      t191 = 0.4006856343865313D0 + t3 * 0.5772156649015329D0 / 0.12D2 +
     # t182 * 0.5772156649015329D0 / 0.6D1
      t196 = (t3 / 0.12D2 + t182 / 0.2D1) ** 2
      t207 = (t48 * t50 - 0.120D3 * wd * (t56 * t58 / 0.2D1 - t56 * t55 
     #* t50 / 0.6D1) * 0.3141592653589793D1 + t70 * wd * (-t55 * t58 + t
     #56 * t50 / 0.2D1) + t79 * wd * (-0.1D1 - t49 + t57 - t55 * t50)) *
     # t85 / 0.40D2 - (-((-t5 - t18 + 0.2D1 * t90 + 0.120D3 * t93 + t38 
     #- t95 + t96 / 0.2D1 + 0.20D2 * t99 - t103 / 0.2D1 - (t39 - t40 - 0
     #.120D3 * t88) * t4 / 0.6D1) * t110 * t58 + t173 * t110 * t50) * 0.
     #3141592653589793D1 + 0.120D3 * t178 * (0.3141592653589793D1 * (t11
     #6 / 0.80D2 + 0.8013712687730627D0 * 0.5772156649015329D0 + t3 * t1
     #82 / 0.12D2 + t185 / 0.12D2 - 0.2D1 * 0.5772156649015329D0 * t191 
     #+ t196) - 0.7D1 / 0.360D3 * t117)) * wd / 0.80D2
      t208 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t207)
      t210 = -0.1D1 + x1
      t214 = x1 * z
      t215 = 0.2D1 * t214
      t217 = 0.2D1 * t52 * z
      t218 = t52 * t49
      t219 = x1 * t49
      t220 = log(x1)
      t223 = x1 - t215 + t217 - t52 + 0.1D1 - t218 + t49 - t57 + t219 - 
     #0.2D1 * t220 * t178
      t225 = -x1 + t214 + 0.1D1
      t228 = -z - x1 + t214
      t230 = z / t225 / t228
      t231 = polylog(2, -t230)
      t233 = t210 ** 2
      t234 = 0.1D1 / t233
      t236 = log(-t225 * t228 * t234)
      t237 = t236 ** 2
      t242 = log(t234 * z)
      t243 = t242 ** 2
      t245 = -t4 / 0.6D1 + (t231 + t237 / 0.2D1) * 0.3141592653589793D1 
     #- t243 * 0.3141592653589793D1
      t247 = x1 - t52 + 0.1D1 - t215 + t217 - t57 + t219 - t218 + t49
      t249 = t220 ** 2
      t252 = -0.2D1 * t220 * t247 + 0.2D1 * t249 * t178
      t256 = -0.2D1 * t242 * 0.3141592653589793D1 + t236 * 0.31415926535
     #89793D1
      t258 = 0.1D1 + t230
      t259 = log(t258)
      t262 = t259 ** 2
      t263 = log(-t230)
      t267 = polylog(3, t258)
      t268 = polylog(3, -t230)
      t304 = t48 * t178 + 0.120D3 * wd * (t223 * t245 + t252 * t256 + t1
     #78 * ((-t3 * t259 / 0.6D1 + t262 * t263 / 0.2D1 + t259 * t231 + t2
     #67 + t268 - 0.1202056903159594D1 + t236 * t231 + t237 * t236 / 0.6
     #D1) * 0.3141592653589793D1 - t236 * t4 / 0.6D1 - t243 * t242 * 0.3
     #141592653589793D1 / 0.3D1) - (0.2D1 * t249 * t247 - 0.4D1 / 0.3D1 
     #* t249 * t220 * t178) * 0.3141592653589793D1) - t70 * t110 * wd * 
     #(t223 * t256 + t178 * t245 - t252 * 0.3141592653589793D1) - t79 * 
     #t110 * wd * (t178 * t256 - t223 * 0.3141592653589793D1)
      t307 = FJET(XB1, XB2, s, -t2 * t210, t2 * x1, 0.0D0, 0.0D0, 0.0D0,
     # t304 * t85 / 0.40D2)
      RVbbargH4n1e1 = t208 * t207 + t307 * t304 * t85 / 0.40D2

      end function



      doubleprecision function RVbbargH4n1e0
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t2 = s * (-0.1D1 + z)
      t3 = log(z)
      t4 = 0.120D3 * t3
      t5 = 0.240D3 * lh
      t6 = -t4 + t5 - 0.240D3
      t7 = 0.3141592653589793D1 * t6
      t8 = 0.2D1 * t7
      t9 = 0.1D1 / z
      t10 = log(t9)
      t11 = t10 * 0.3141592653589793D1
      t13 = 0.3141592653589793D1 ** 2
      t14 = t13 * 0.3141592653589793D1
      t15 = 0.40D2 * t14
      t16 = lh ** 2
      t18 = t3 ** 2
      t20 = t3 * lh
      t25 = -0.480D3 - 0.240D3 * t16 - 0.60D2 * t18 + 0.240D3 * t20 + 0.
     #20D2 * t13 - 0.240D3 * t3 + 0.480D3 * lh
      t26 = 0.3141592653589793D1 * t25
      t27 = t11 * t6
      t28 = t10 ** 2
      t29 = t28 * 0.3141592653589793D1
      t32 = 0.1D1 / 0.3141592653589793D1
      t34 = z ** 2
      t35 = 0.2D1 * z
      t36 = -0.1D1 - t34 + t35
      t60 = -0.5753417909889299D3 - 0.20D2 * t18 * t3 + 0.120D3 * t18 * 
     #lh + 0.160D3 * t16 * lh - 0.120D3 * t18 - 0.40D2 * t13 * lh - 0.48
     #0D3 * t16 + 0.480D3 * t20 - 0.240D3 * t3 * t16 - 0.480D3 * t3 + 0.
     #960D3 * lh + 0.20D2 * t13 * t3 + 0.40D2 * t13
      t78 = 0.1D1 + t34
      t85 = 0.240D3 * 0.3141592653589793D1 + t7
      t87 = x1 ** 2
      t89 = log(t87 * t9)
      t94 = (-t8 + t15 + t26) * wd
      t97 = t89 ** 2
      t105 = 0.1D1 / x1
      t108 = ((-t8 - 0.240D3 * t11 + t15 + t26 - t27 - 0.60D2 * t29) * t
     #32 * t36 + (-0.120D3 * t14 - 0.2D1 * t26 + 0.2D1 * t27 + 0.120D3 *
     # t29 + 0.3141592653589793D1 * t60 - t11 * t25 + t29 * t6 / 0.2D1 +
     # 0.20D2 * t28 * t10 * 0.3141592653589793D1 - (t7 + 0.120D3 * t11) 
     #* t13 / 0.2D1 - (t4 - t5 - 0.120D3 * t10) * t14 / 0.6D1) * t32 * t
     #78) * 0.3141592653589793D1 * wd / 0.80D2 + (t85 * wd * (-0.1D1 - t
     #34 + t35 - t89 * t78) + t94 * t78 - 0.120D3 * wd * (-t89 * t36 + t
     #97 * t78 / 0.2D1) * 0.3141592653589793D1) * t105 / 0.40D2
      t109 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t108)
      t111 = -0.1D1 + x1
      t115 = -t78
      t116 = t111 ** 2
      t117 = 0.1D1 / t116
      t119 = log(t117 * z)
      t122 = x1 * z
      t123 = -x1 + t122 + 0.1D1
      t124 = -z - x1 + t122
      t127 = log(-t123 * t124 * t117)
      t129 = -0.2D1 * t119 * 0.3141592653589793D1 + t127 * 0.31415926535
     #89793D1
      t131 = 0.2D1 * t122
      t133 = 0.2D1 * t87 * z
      t134 = t87 * t34
      t135 = x1 * t34
      t136 = log(x1)
      t139 = x1 - t131 + t133 - t87 + 0.1D1 - t134 + t34 - t35 + t135 - 
     #0.2D1 * t136 * t115
      t151 = polylog(2, -z / t123 / t124)
      t152 = t127 ** 2
      t156 = t119 ** 2
      t162 = t136 ** 2
      t170 = -t85 * t32 * wd * (t115 * t129 - t139 * 0.3141592653589793D
     #1) + t94 * t115 + 0.120D3 * wd * (t139 * t129 + t115 * (-t14 / 0.6
     #D1 + (t151 + t152 / 0.2D1) * 0.3141592653589793D1 - t156 * 0.31415
     #92653589793D1) - (-0.2D1 * t136 * (x1 - t87 + 0.1D1 - t131 + t133 
     #- t35 + t135 - t134 + t34) + 0.2D1 * t162 * t115) * 0.314159265358
     #9793D1)
      t173 = FJET(XB1, XB2, s, -t2 * t111, t2 * x1, 0.0D0, 0.0D0, 0.0D0,
     # t170 * t105 / 0.40D2)
      RVbbargH4n1e0 = t109 * t108 + t173 * t170 * t105 / 0.40D2

      end function



      doubleprecision function RVbbargH4n1em1
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t2 = s * (-0.1D1 + z)
      t3 = z ** 2
      t4 = 0.2D1 * z
      t5 = x1 ** 2
      t6 = 0.1D1 / z
      t8 = log(t5 * t6)
      t9 = 0.1D1 + t3
      t15 = 0.240D3 * 0.3141592653589793D1
      t16 = log(z)
      t19 = -0.120D3 * t16 + 0.240D3 * lh - 0.240D3
      t20 = 0.3141592653589793D1 * t19
      t22 = (t15 + t20) * wd
      t25 = 0.1D1 / x1
      t28 = log(t6)
      t29 = t28 * 0.3141592653589793D1
      t32 = 0.1D1 / 0.3141592653589793D1
      t38 = 0.3141592653589793D1 ** 2
      t41 = lh ** 2
      t43 = t16 ** 2
      t53 = t28 ** 2
      t63 = (-0.120D3 * wd * (-0.1D1 - t3 + t4 - t9 * t8) * 0.3141592653
     #589793D1 + t22 * t9) * t25 / 0.40D2 + ((t15 + t20 + 0.120D3 * t29)
     # * t32 * (-0.1D1 - t3 + t4) + (-0.2D1 * t20 - 0.240D3 * t29 + 0.40
     #D2 * t38 * 0.3141592653589793D1 + 0.3141592653589793D1 * (-0.480D3
     # - 0.240D3 * t41 - 0.60D2 * t43 + 0.240D3 * t16 * lh + 0.20D2 * t3
     #8 - 0.240D3 * t16 + 0.480D3 * lh) - t29 * t19 - 0.60D2 * t53 * 0.3
     #141592653589793D1) * t32 * t9) * 0.3141592653589793D1 * wd / 0.80D
     #2
      t64 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t63)
      t66 = -0.1D1 + x1
      t69 = -t9
      t70 = t66 ** 2
      t71 = 0.1D1 / t70
      t73 = log(t71 * z)
      t76 = x1 * z
      t81 = log(-(-x1 + t76 + 0.1D1) * (-z - x1 + t76) * t71)
      t90 = log(x1)
      t99 = 0.120D3 * wd * (t69 * (-0.2D1 * t73 * 0.3141592653589793D1 +
     # t81 * 0.3141592653589793D1) - (x1 - 0.2D1 * t76 + 0.2D1 * t5 * z 
     #- t5 + 0.1D1 - t5 * t3 + t3 - t4 + x1 * t3 - 0.2D1 * t90 * t69) * 
     #0.3141592653589793D1) + t22 * t69
      t102 = FJET(XB1, XB2, s, -t2 * t66, t2 * x1, 0.0D0, 0.0D0, 0.0D0, 
     #t99 * t25 / 0.40D2)
      RVbbargH4n1em1 = t64 * t63 + t102 * t99 * t25 / 0.40D2

      end function



      doubleprecision function RVbbargH4n1em2
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t2 = s * (-0.1D1 + z)
      t3 = z ** 2
      t4 = 0.1D1 + t3
      t6 = 0.1D1 / x1
      t7 = 0.3141592653589793D1 * t6
      t13 = log(z)
      t19 = log(0.1D1 / z)
      t30 = -0.3D1 * wd * t4 * t7 + (0.120D3 + 0.120D3 * t3 - 0.240D3 * 
     #z + (0.240D3 * 0.3141592653589793D1 + 0.3141592653589793D1 * (-0.1
     #20D3 * t13 + 0.240D3 * lh - 0.240D3) + 0.120D3 * 0.314159265358979
     #3D1 * t19) / 0.3141592653589793D1 * t4) * 0.3141592653589793D1 * w
     #d / 0.80D2
      t31 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t30)
      t36 = -t4
      t40 = FJET(XB1, XB2, s, -t2 * (-0.1D1 + x1), t2 * x1, 0.0D0, 0.0D0
     #, 0.0D0, -0.3D1 * wd * t36 * t7)
      RVbbargH4n1em2 = t31 * t30 - 0.3D1 * t40 * wd * t36 * 0.3141592653
     #589793D1 * t6

      end function



      doubleprecision function RVbbargH4n1em3
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t3 = z ** 2
      t5 = -0.120D3 - 0.120D3 * t3
      t9 = FJET(XB1, XB2, s, s * (-0.1D1 + z), 0.0D0, 0.0D0, 0.0D0, 0.0D
     #0, t5 * 0.3141592653589793D1 * wd / 0.80D2)
      RVbbargH4n1em3 = t9 * t5 * 0.3141592653589793D1 * wd / 0.80D2

      end function



      doubleprecision function RVbbargH4n1em4
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      RVbbargH4n1em4 = 0.0D0

      end function


      doubleprecision function RVbbargH4n2e1
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t2 = s * (-0.1D1 + z)
      t3 = 0.3141592653589793D1 ** 2
      t4 = t3 * 0.3141592653589793D1
      t5 = 0.120D3 * t4
      t6 = lh ** 2
      t7 = 0.240D3 * t6
      t8 = log(z)
      t9 = t8 ** 2
      t10 = 0.60D2 * t9
      t11 = t8 * lh
      t12 = 0.240D3 * t11
      t16 = -0.480D3 - t7 - t10 + t12 + 0.20D2 * t3 - 0.240D3 * t8 + 0.4
     #80D3 * lh
      t17 = 0.3141592653589793D1 * t16
      t18 = 0.2D1 * t17
      t19 = t9 * t8
      t21 = t9 * lh
      t23 = t6 * lh
      t26 = t3 * lh
      t30 = t8 * t6
      t34 = t3 * t8
      t37 = -0.5753417909889299D3 - 0.20D2 * t19 + 0.120D3 * t21 + 0.160
     #D3 * t23 - 0.120D3 * t9 - 0.40D2 * t26 - 0.480D3 * t6 + 0.480D3 * 
     #t11 - 0.240D3 * t30 - 0.480D3 * t8 + 0.960D3 * lh + 0.20D2 * t34 +
     # 0.40D2 * t3
      t38 = 0.3141592653589793D1 * t37
      t39 = 0.120D3 * t8
      t40 = 0.240D3 * lh
      t41 = -t39 + t40 - 0.240D3
      t48 = (-t5 - t18 + t38 - t4 * t41 / 0.2D1 - (t39 - t40) * t4 / 0.6
     #D1) * wd
      t49 = z ** 2
      t50 = 0.1D1 + t49
      t52 = x1 ** 2
      t53 = 0.1D1 / z
      t55 = log(t52 * t53)
      t56 = t55 ** 2
      t57 = 0.2D1 * z
      t58 = -0.1D1 - t49 + t57
      t69 = 0.3141592653589793D1 * t41
      t70 = 0.240D3 * 0.3141592653589793D1 + t69
      t79 = -0.2D1 * t69 + 0.40D2 * t4 + t17
      t85 = 0.1D1 / x1
      t88 = log(t53)
      t89 = t88 * 0.3141592653589793D1
      t90 = t89 * t41
      t92 = t88 ** 2
      t93 = t92 * 0.3141592653589793D1
      t95 = t89 * t16
      t96 = t93 * t41
      t99 = t92 * t88 * 0.3141592653589793D1
      t103 = (t69 + 0.120D3 * t89) * t3
      t110 = 0.1D1 / 0.3141592653589793D1
      t116 = t3 ** 2
      t117 = t116 * 0.3141592653589793D1
      t125 = t9 ** 2
      t129 = t6 ** 2
      t132 = 0.1150683581977860D4 * lh + 0.80D2 * t3 - 0.960D3 * t6 - 0.
     #5753417909889299D3 * t8 - 0.240D3 * t9 - 0.40D2 * t19 - 0.5D1 * t1
     #25 + 0.3D1 * t116 + 0.320D3 * t23 - 0.80D2 * t129 + 0.40D2 * t34
      t149 = -0.1150683581977860D4 + 0.40D2 * t3 * t6 + 0.10D2 * t3 * t9
     # + 0.960D3 * t11 + 0.160D3 * t8 * t23 - 0.120D3 * t9 * t6 + 0.40D2
     # * t19 * lh + 0.240D3 * t21 - 0.480D3 * t30 - 0.80D2 * t26 - 0.40D
     #2 * t34 * lh
      t157 = t92 ** 2
      t173 = -0.2D1 * t38 + 0.2D1 * t95 - t96 - 0.40D2 * t99 + t103 - 0.
     #4D1 * t117 + 0.3141592653589793D1 * (t132 + t149) - t89 * t37 + t9
     #3 * t16 / 0.2D1 - t99 * t41 / 0.6D1 - 0.5D1 * t157 * 0.31415926535
     #89793D1 - (t17 - t90 - 0.60D2 * t93) * t3 / 0.2D1 - (-0.60D2 * t3 
     #+ t7 + t10 - t12 - 0.120D3 * t88 * t8 + 0.240D3 * t88 * lh + 0.60D
     #2 * t92) * t4 / 0.6D1
      t178 = -t50
      t182 = 0.5772156649015329D0 ** 2
      t185 = t182 ** 2
      t191 = 0.4006856343865313D0 + t3 * 0.5772156649015329D0 / 0.12D2 +
     # t182 * 0.5772156649015329D0 / 0.6D1
      t196 = (t3 / 0.12D2 + t182 / 0.2D1) ** 2
      t207 = (t48 * t50 - 0.120D3 * wd * (t56 * t58 / 0.2D1 - t56 * t55 
     #* t50 / 0.6D1) * 0.3141592653589793D1 + t70 * wd * (-t55 * t58 + t
     #56 * t50 / 0.2D1) + t79 * wd * (-0.1D1 - t49 + t57 - t55 * t50)) *
     # t85 / 0.40D2 - (-((-t5 - t18 + 0.2D1 * t90 + 0.120D3 * t93 + t38 
     #- t95 + t96 / 0.2D1 + 0.20D2 * t99 - t103 / 0.2D1 - (t39 - t40 - 0
     #.120D3 * t88) * t4 / 0.6D1) * t110 * t58 + t173 * t110 * t50) * 0.
     #3141592653589793D1 + 0.120D3 * t178 * (0.3141592653589793D1 * (t11
     #6 / 0.80D2 + 0.8013712687730627D0 * 0.5772156649015329D0 + t3 * t1
     #82 / 0.12D2 + t185 / 0.12D2 - 0.2D1 * 0.5772156649015329D0 * t191 
     #+ t196) - 0.7D1 / 0.360D3 * t117)) * wd / 0.80D2
      t208 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t207)
      t211 = -0.1D1 + x1
      t214 = x1 * z
      t215 = 0.2D1 * t214
      t217 = 0.2D1 * t52 * z
      t218 = t52 * t49
      t219 = x1 * t49
      t220 = log(x1)
      t223 = x1 - t215 + t217 - t52 + 0.1D1 - t218 + t49 - t57 + t219 - 
     #0.2D1 * t220 * t178
      t225 = -x1 + t214 + 0.1D1
      t228 = -z - x1 + t214
      t230 = z / t225 / t228
      t231 = polylog(2, -t230)
      t233 = t211 ** 2
      t234 = 0.1D1 / t233
      t236 = log(-t225 * t228 * t234)
      t237 = t236 ** 2
      t242 = log(t234 * z)
      t243 = t242 ** 2
      t245 = -t4 / 0.6D1 + (t231 + t237 / 0.2D1) * 0.3141592653589793D1 
     #- t243 * 0.3141592653589793D1
      t247 = x1 - t52 + 0.1D1 - t215 + t217 - t57 + t219 - t218 + t49
      t249 = t220 ** 2
      t252 = -0.2D1 * t220 * t247 + 0.2D1 * t249 * t178
      t256 = -0.2D1 * t242 * 0.3141592653589793D1 + t236 * 0.31415926535
     #89793D1
      t258 = 0.1D1 + t230
      t259 = log(t258)
      t262 = t259 ** 2
      t263 = log(-t230)
      t267 = polylog(3, t258)
      t268 = polylog(3, -t230)
      t304 = t48 * t178 + 0.120D3 * wd * (t223 * t245 + t252 * t256 + t1
     #78 * ((-t3 * t259 / 0.6D1 + t262 * t263 / 0.2D1 + t259 * t231 + t2
     #67 + t268 - 0.1202056903159594D1 + t236 * t231 + t237 * t236 / 0.6
     #D1) * 0.3141592653589793D1 - t236 * t4 / 0.6D1 - t243 * t242 * 0.3
     #141592653589793D1 / 0.3D1) - (0.2D1 * t249 * t247 - 0.4D1 / 0.3D1 
     #* t249 * t220 * t178) * 0.3141592653589793D1) - t70 * t110 * wd * 
     #(t223 * t256 + t178 * t245 - t252 * 0.3141592653589793D1) - t79 * 
     #t110 * wd * (t178 * t256 - t223 * 0.3141592653589793D1)
      t307 = FJET(XB1, XB2, s, t2 * x1, -t2 * t211, 0.0D0, 0.0D0, 0.0D0,
     # t304 * t85 / 0.40D2)
      RVbbargH4n2e1 = t208 * t207 + t307 * t304 * t85 / 0.40D2

      end function



      doubleprecision function RVbbargH4n2e0
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t2 = s * (-0.1D1 + z)
      t3 = log(z)
      t4 = 0.120D3 * t3
      t5 = 0.240D3 * lh
      t6 = -t4 + t5 - 0.240D3
      t7 = 0.3141592653589793D1 * t6
      t8 = 0.2D1 * t7
      t9 = 0.1D1 / z
      t10 = log(t9)
      t11 = t10 * 0.3141592653589793D1
      t13 = 0.3141592653589793D1 ** 2
      t14 = t13 * 0.3141592653589793D1
      t15 = 0.40D2 * t14
      t16 = lh ** 2
      t18 = t3 ** 2
      t20 = t3 * lh
      t25 = -0.480D3 - 0.240D3 * t16 - 0.60D2 * t18 + 0.240D3 * t20 + 0.
     #20D2 * t13 - 0.240D3 * t3 + 0.480D3 * lh
      t26 = 0.3141592653589793D1 * t25
      t27 = t11 * t6
      t28 = t10 ** 2
      t29 = t28 * 0.3141592653589793D1
      t32 = 0.1D1 / 0.3141592653589793D1
      t34 = z ** 2
      t35 = 0.2D1 * z
      t36 = -0.1D1 - t34 + t35
      t60 = -0.5753417909889299D3 - 0.20D2 * t18 * t3 + 0.120D3 * t18 * 
     #lh + 0.160D3 * t16 * lh - 0.120D3 * t18 - 0.40D2 * t13 * lh - 0.48
     #0D3 * t16 + 0.480D3 * t20 - 0.240D3 * t3 * t16 - 0.480D3 * t3 + 0.
     #960D3 * lh + 0.20D2 * t13 * t3 + 0.40D2 * t13
      t78 = 0.1D1 + t34
      t85 = 0.240D3 * 0.3141592653589793D1 + t7
      t87 = x1 ** 2
      t89 = log(t87 * t9)
      t94 = (-t8 + t15 + t26) * wd
      t97 = t89 ** 2
      t105 = 0.1D1 / x1
      t108 = ((-t8 - 0.240D3 * t11 + t15 + t26 - t27 - 0.60D2 * t29) * t
     #32 * t36 + (-0.120D3 * t14 - 0.2D1 * t26 + 0.2D1 * t27 + 0.120D3 *
     # t29 + 0.3141592653589793D1 * t60 - t11 * t25 + t29 * t6 / 0.2D1 +
     # 0.20D2 * t28 * t10 * 0.3141592653589793D1 - (t7 + 0.120D3 * t11) 
     #* t13 / 0.2D1 - (t4 - t5 - 0.120D3 * t10) * t14 / 0.6D1) * t32 * t
     #78) * 0.3141592653589793D1 * wd / 0.80D2 + (t85 * wd * (-0.1D1 - t
     #34 + t35 - t89 * t78) + t94 * t78 - 0.120D3 * wd * (-t89 * t36 + t
     #97 * t78 / 0.2D1) * 0.3141592653589793D1) * t105 / 0.40D2
      t109 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t108)
      t112 = -0.1D1 + x1
      t115 = -t78
      t116 = t112 ** 2
      t117 = 0.1D1 / t116
      t119 = log(t117 * z)
      t122 = x1 * z
      t123 = -x1 + t122 + 0.1D1
      t124 = -z - x1 + t122
      t127 = log(-t123 * t124 * t117)
      t129 = -0.2D1 * t119 * 0.3141592653589793D1 + t127 * 0.31415926535
     #89793D1
      t131 = 0.2D1 * t122
      t133 = 0.2D1 * t87 * z
      t134 = t87 * t34
      t135 = x1 * t34
      t136 = log(x1)
      t139 = x1 - t131 + t133 - t87 + 0.1D1 - t134 + t34 - t35 + t135 - 
     #0.2D1 * t136 * t115
      t151 = polylog(2, -z / t123 / t124)
      t152 = t127 ** 2
      t156 = t119 ** 2
      t162 = t136 ** 2
      t170 = -t85 * t32 * wd * (t115 * t129 - t139 * 0.3141592653589793D
     #1) + t94 * t115 + 0.120D3 * wd * (t139 * t129 + t115 * (-t14 / 0.6
     #D1 + (t151 + t152 / 0.2D1) * 0.3141592653589793D1 - t156 * 0.31415
     #92653589793D1) - (-0.2D1 * t136 * (x1 - t87 + 0.1D1 - t131 + t133 
     #- t35 + t135 - t134 + t34) + 0.2D1 * t162 * t115) * 0.314159265358
     #9793D1)
      t173 = FJET(XB1, XB2, s, t2 * x1, -t2 * t112, 0.0D0, 0.0D0, 0.0D0,
     # t170 * t105 / 0.40D2)
      RVbbargH4n2e0 = t109 * t108 + t173 * t170 * t105 / 0.40D2

      end function



      doubleprecision function RVbbargH4n2em1
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t2 = s * (-0.1D1 + z)
      t3 = z ** 2
      t4 = 0.2D1 * z
      t5 = x1 ** 2
      t6 = 0.1D1 / z
      t8 = log(t5 * t6)
      t9 = 0.1D1 + t3
      t15 = 0.240D3 * 0.3141592653589793D1
      t16 = log(z)
      t19 = -0.120D3 * t16 + 0.240D3 * lh - 0.240D3
      t20 = 0.3141592653589793D1 * t19
      t22 = (t15 + t20) * wd
      t25 = 0.1D1 / x1
      t28 = log(t6)
      t29 = t28 * 0.3141592653589793D1
      t32 = 0.1D1 / 0.3141592653589793D1
      t38 = 0.3141592653589793D1 ** 2
      t41 = lh ** 2
      t43 = t16 ** 2
      t53 = t28 ** 2
      t63 = (-0.120D3 * wd * (-0.1D1 - t3 + t4 - t9 * t8) * 0.3141592653
     #589793D1 + t22 * t9) * t25 / 0.40D2 + ((t15 + t20 + 0.120D3 * t29)
     # * t32 * (-0.1D1 - t3 + t4) + (-0.2D1 * t20 - 0.240D3 * t29 + 0.40
     #D2 * t38 * 0.3141592653589793D1 + 0.3141592653589793D1 * (-0.480D3
     # - 0.240D3 * t41 - 0.60D2 * t43 + 0.240D3 * t16 * lh + 0.20D2 * t3
     #8 - 0.240D3 * t16 + 0.480D3 * lh) - t29 * t19 - 0.60D2 * t53 * 0.3
     #141592653589793D1) * t32 * t9) * 0.3141592653589793D1 * wd / 0.80D
     #2
      t64 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t63)
      t67 = -0.1D1 + x1
      t69 = -t9
      t70 = t67 ** 2
      t71 = 0.1D1 / t70
      t73 = log(t71 * z)
      t76 = x1 * z
      t81 = log(-(-x1 + t76 + 0.1D1) * (-z - x1 + t76) * t71)
      t90 = log(x1)
      t99 = 0.120D3 * wd * (t69 * (-0.2D1 * t73 * 0.3141592653589793D1 +
     # t81 * 0.3141592653589793D1) - (x1 - 0.2D1 * t76 + 0.2D1 * t5 * z 
     #- t5 + 0.1D1 - t5 * t3 + t3 - t4 + x1 * t3 - 0.2D1 * t90 * t69) * 
     #0.3141592653589793D1) + t22 * t69
      t102 = FJET(XB1, XB2, s, t2 * x1, -t2 * t67, 0.0D0, 0.0D0, 0.0D0, 
     #t99 * t25 / 0.40D2)
      RVbbargH4n2em1 = t64 * t63 + t102 * t99 * t25 / 0.40D2

      end function



      doubleprecision function RVbbargH4n2em2
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t2 = s * (-0.1D1 + z)
      t3 = z ** 2
      t4 = 0.1D1 + t3
      t6 = 0.1D1 / x1
      t7 = 0.3141592653589793D1 * t6
      t13 = log(z)
      t19 = log(0.1D1 / z)
      t30 = -0.3D1 * wd * t4 * t7 + (0.120D3 + 0.120D3 * t3 - 0.240D3 * 
     #z + (0.240D3 * 0.3141592653589793D1 + 0.3141592653589793D1 * (-0.1
     #20D3 * t13 + 0.240D3 * lh - 0.240D3) + 0.120D3 * 0.314159265358979
     #3D1 * t19) / 0.3141592653589793D1 * t4) * 0.3141592653589793D1 * w
     #d / 0.80D2
      t31 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t30)
      t36 = -t4
      t40 = FJET(XB1, XB2, s, t2 * x1, -t2 * (-0.1D1 + x1), 0.0D0, 0.0D0
     #, 0.0D0, -0.3D1 * wd * t36 * t7)
      RVbbargH4n2em2 = t31 * t30 - 0.3D1 * t40 * wd * t36 * 0.3141592653
     #589793D1 * t6

      end function



      doubleprecision function RVbbargH4n2em3
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t3 = z ** 2
      t5 = -0.120D3 - 0.120D3 * t3
      t9 = FJET(XB1, XB2, s, 0.0D0, s * (-0.1D1 + z), 0.0D0, 0.0D0, 0.0D
     #0, t5 * 0.3141592653589793D1 * wd / 0.80D2)
      RVbbargH4n2em3 = t9 * t5 * 0.3141592653589793D1 * wd / 0.80D2

      end function



      doubleprecision function RVbbargH4n2em4
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      RVbbargH4n2em4 = 0.0D0

      end function
