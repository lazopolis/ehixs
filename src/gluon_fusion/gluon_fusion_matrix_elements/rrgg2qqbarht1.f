  
      subroutine rrgg2qqbarht1
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2qqbarh11J1  
      doubleprecision rrgg2qqbarh11J2  
      doubleprecision rrgg2qqbarh11J3  
      doubleprecision rrgg2qqbarh11J4  
      doubleprecision rrgg2qqbarh11J5  
      doubleprecision rrgg2qqbarh11J6  
      doubleprecision rrgg2qqbarh11J7  
      doubleprecision rrgg2qqbarht1s1e1  
      doubleprecision rrgg2qqbarht1s1e0  
      doubleprecision rrgg2qqbarht1s1em1  
      doubleprecision rrgg2qqbarht1s1em2  
      doubleprecision rrgg2qqbarht1s1em3  
      doubleprecision rrgg2qqbarht1s1em4  
      doubleprecision rrgg2qqbarht1s2e1  
      doubleprecision rrgg2qqbarht1s2e0  
      doubleprecision rrgg2qqbarht1s2em1  
      doubleprecision rrgg2qqbarht1s2em2  
      doubleprecision rrgg2qqbarht1s2em3  
      doubleprecision rrgg2qqbarht1s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht1s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht1s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht1s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht1s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht1s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht1s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht1s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht1s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht1s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht1s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht1s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht1s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2qqbarht1s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh11J1
      doubleprecision rrgg2qqbarh11J2
      doubleprecision rrgg2qqbarh11J3
      doubleprecision rrgg2qqbarh11J4
      doubleprecision rrgg2qqbarh11J5
      doubleprecision rrgg2qqbarh11J6
      doubleprecision rrgg2qqbarh11J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = 0.3141592653589793D1 * t7
      t9 = rrgg2qqbarh11J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3
     #, x4)
      t10 = x4 * 0.3141592653589793D1
      t11 = Sin(t10)
      t12 = t11 ** 2
      t13 = x2 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = t15 * x3
      t17 = t16 * t4
      t20 = log(-0.4D1 * t13 * t17)
      t21 = rrgg2qqbarh11J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x
     #3, x4)
      t23 = t20 ** 2
      t24 = rrgg2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x
     #3, x4)
      t30 = 0.3141592653589793D1 * lh
      t36 = lh ** 2
      t38 = 0.3141592653589793D1 ** 2
      t40 = 0.180D3 * t36 - 0.30D2 * t38
      t41 = 0.3141592653589793D1 * t40
      t42 = t7 * t24
      t43 = t41 * t42
      t45 = 0.1D1 / x2
      t48 = t12 * t15
      t49 = x3 * t4
      t52 = log(-0.4D1 * t48 * t49)
      t53 = t52 * 0.3141592653589793D1
      t56 = t52 ** 2
      t57 = t56 * 0.3141592653589793D1
      t63 = rrgg2qqbarh11J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x
     #3, x4)
      t88 = x1 ** 2
      t89 = x2 * t88
      t90 = t89 * t12
      t93 = log(-0.4D1 * t90 * t17)
      t101 = 0.1D1 / x1
      t105 = t88 * t12
      t108 = log(-0.4D1 * t105 * t17)
      t110 = t108 ** 2
      t124 = -(0.90D2 * t8 * (-t9 + t20 * t21 - t23 * t24 / 0.2D1) - 0.1
     #80D3 * t30 * t7 * (-t21 + t20 * t24) - t43) * t45 / 0.1440D4 + (0.
     #180D3 * t53 * lh + 0.45D2 * t57 + t41) * t7 * t21 / 0.1440D4 + t8 
     #* t63 / 0.16D2 + (-0.90D2 * t57 * lh + 0.3141592653589793D1 * (0.6
     #0D2 * lh * t38 - 0.2884936567583026D3 - 0.120D3 * t36 * lh) - 0.15
     #D2 * t56 * t52 * 0.3141592653589793D1 - t53 * t40) * t7 * t24 / 0.
     #1440D4 + (-0.180D3 * t30 - 0.90D2 * t53) * t7 * t9 / 0.1440D4 - (0
     #.90D2 * t8 * (-t21 + t93 * t24) + 0.180D3 * t30 * t42) * t101 * t4
     #5 / 0.720D3 - (0.90D2 * t8 * (-t9 + t108 * t21 - t110 * t24 / 0.2D
     #1) - 0.180D3 * t30 * t7 * (-t21 + t108 * t24) - t43) * t101 / 0.72
     #0D3
      t125 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t
     #124)
      t127 = -0.1D1 + x1
      t128 = x3 * x1
      t129 = t128 * z
      t131 = 0.2D1 * x2 * x3
      t132 = t128 * x2
      t133 = x2 * z
      t134 = t128 * t133
      t135 = cos(t10)
      t136 = -0.1D1 + x2
      t138 = x1 * z
      t139 = 0.1D1 - x1 + t138
      t143 = Sqrt(x3 * t136 * t139 * x2 * t4)
      t145 = 0.2D1 * t135 * t143
      t148 = 0.1D1 / t139
      t151 = t2 * t128
      t152 = x1 * x2
      t153 = t152 * z
      t154 = 0.1D1 - x1 + t138 - x2 + t152 - t153 - x3 + t128 - t129 + t
     #131 - t132 + t134 + t145
      t159 = t2 * x1 * t4
      t160 = t1 ** 2
      t167 = 0.1D1 / (-0.1D1 + x1 - t138 + x2 - t152 - t133 + t153)
      t168 = t139 * t167
      t169 = -t127
      t170 = rrgg2qqbarh11J2(s, XB1, XB2, z, lh, wd, nf, t169, x2, x3, x
     #4)
      t172 = t89 * t48
      t173 = t127 ** 2
      t174 = t148 * t173
      t179 = log(0.4D1 * t172 * t49 * t174 * t136)
      t181 = rrgg2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, t169, x2, x3, x
     #4)
      t191 = 0.90D2 * t8 * (t168 * t170 - t179 * t139 * t167 * t181) - 0
     #.180D3 * t30 * t7 * t168 * t181
      t195 = FJET(XB1, XB2, s, t2 * t127 * (-x3 + t128 - t129 + t131 - t
     #132 + t134 - x2 + t145) * t148, t151, -t2 * t127 * t154 * t148, -t
     #159, -s * t160 * x2 * x1 * t127 * t148, -t191 * t101 * t45 / 0.720
     #D3)
      t202 = Sqrt(x2 * t136 * t49)
      t204 = 0.2D1 * t135 * t202
      t210 = 0.1D1 / (0.1D1 - x2 + t133)
      t211 = rrgg2qqbarh11J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3,
     # x4)
      t217 = log(0.4D1 * t13 * t15 * t49 * t136)
      t218 = t217 * t210
      t219 = rrgg2qqbarh11J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3,
     # x4)
      t221 = t217 ** 2
      t223 = rrgg2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3,
     # x4)
      t229 = t210 * t219
      t236 = t7 * t210 * t223
      t245 = log(0.4D1 * t90 * t16 * t4 * t136)
      t257 = -(0.90D2 * t8 * (t210 * t211 - t218 * t219 + t221 * t210 * 
     #t223 / 0.2D1) - 0.180D3 * t30 * t7 * (t229 - t218 * t223) + t41 * 
     #t236) * t45 / 0.1440D4 - (0.90D2 * t8 * (t229 - t245 * t210 * t223
     #) - 0.180D3 * t30 * t236) * t101 * t45 / 0.720D3
      t258 = FJET(XB1, XB2, s, -t2 * (-x3 + t131 - x2 + t204), 0.0D0, t2
     # * (0.1D1 - x2 - x3 + t131 + t204), 0.0D0, 0.0D0, t257)
      t264 = rrgg2qqbarh11J2(s, XB1, XB2, z, lh, wd, nf, t169, 0.0D0, x3
     #, x4)
      t265 = t49 * t174
      t268 = log(-0.4D1 * t172 * t265)
      t269 = rrgg2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, t169, 0.0D0, x3
     #, x4)
      t274 = t7 * t269
      t280 = rrgg2qqbarh11J3(s, XB1, XB2, z, lh, wd, nf, t169, 0.0D0, x3
     #, x4)
      t284 = log(-0.4D1 * t105 * t15 * t265)
      t286 = t284 ** 2
      t301 = -(0.90D2 * t8 * (t264 - t268 * t269) - 0.180D3 * t30 * t274
     #) * t101 * t45 / 0.720D3 - (0.90D2 * t8 * (t280 - t284 * t264 + t2
     #86 * t269 / 0.2D1) - 0.180D3 * t30 * t7 * (t264 - t284 * t269) + t
     #41 * t274) * t101 / 0.720D3
      t302 = FJET(XB1, XB2, s, -t2 * t127 * x3, t151, t2 * t127 * t4, -t
     #159, 0.0D0, t301)
      rrgg2qqbarht1s1e1 = t125 * t124 - t195 * t191 * t101 * t45 / 0.720
     #D3 + t258 * t257 + t302 * t301

      end function



      doubleprecision function rrgg2qqbarht1s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh11J1
      doubleprecision rrgg2qqbarh11J2
      doubleprecision rrgg2qqbarh11J3
      doubleprecision rrgg2qqbarh11J4
      doubleprecision rrgg2qqbarh11J5
      doubleprecision rrgg2qqbarh11J6
      doubleprecision rrgg2qqbarh11J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = 0.3141592653589793D1 * t7
      t9 = rrgg2qqbarh11J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3
     #, x4)
      t10 = x4 * 0.3141592653589793D1
      t11 = Sin(t10)
      t12 = t11 ** 2
      t13 = x2 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t17 = t15 * x3 * t4
      t20 = log(-0.4D1 * t13 * t17)
      t21 = rrgg2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x
     #3, x4)
      t26 = 0.3141592653589793D1 * lh
      t29 = 0.180D3 * t26 * t7 * t21
      t31 = 0.1D1 / x2
      t34 = 0.1D1 / x1
      t39 = x1 ** 2
      t40 = t39 * t12
      t43 = log(-0.4D1 * t40 * t17)
      t51 = rrgg2qqbarh11J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x
     #3, x4)
      t56 = x3 * t4
      t59 = log(-0.4D1 * t12 * t15 * t56)
      t60 = t59 * 0.3141592653589793D1
      t68 = t59 ** 2
      t71 = lh ** 2
      t73 = 0.3141592653589793D1 ** 2
      t81 = -(0.90D2 * t8 * (-t9 + t20 * t21) + t29) * t31 / 0.1440D4 + 
     #t8 * t21 * t34 * t31 / 0.8D1 - (0.90D2 * t8 * (-t9 + t43 * t21) + 
     #t29) * t34 / 0.720D3 + t8 * t51 / 0.16D2 + (-0.180D3 * t26 - 0.90D
     #2 * t60) * t7 * t9 / 0.1440D4 + (0.180D3 * t60 * lh + 0.45D2 * t68
     # * 0.3141592653589793D1 + 0.3141592653589793D1 * (0.180D3 * t71 - 
     #0.30D2 * t73)) * t7 * t21 / 0.1440D4
      t82 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t8
     #1)
      t84 = -0.1D1 + x1
      t85 = x3 * x1
      t86 = t85 * z
      t88 = 0.2D1 * x2 * x3
      t89 = t85 * x2
      t90 = x2 * z
      t91 = t85 * t90
      t92 = cos(t10)
      t93 = -0.1D1 + x2
      t95 = x1 * z
      t96 = 0.1D1 - x1 + t95
      t100 = Sqrt(x3 * t93 * t96 * x2 * t4)
      t102 = 0.2D1 * t92 * t100
      t105 = 0.1D1 / t96
      t108 = t2 * t85
      t109 = x1 * x2
      t110 = t109 * z
      t111 = 0.1D1 - x1 + t95 - x2 + t109 - t110 - x3 + t85 - t86 + t88 
     #- t89 + t91 + t102
      t116 = t2 * x1 * t4
      t117 = t1 ** 2
      t126 = -t84
      t127 = rrgg2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, t126, x2, x3, x
     #4)
      t130 = 0.1D1 / (-0.1D1 + x1 - t95 + x2 - t109 - t90 + t110) * t127
     # * t34 * t31
      t133 = FJET(XB1, XB2, s, t2 * t84 * (-x3 + t85 - t86 + t88 - t89 +
     # t91 - x2 + t102) * t105, t108, -t2 * t84 * t111 * t105, -t116, -s
     # * t117 * x2 * x1 * t84 * t105, -t8 * t96 * t130 / 0.8D1)
      t141 = Sqrt(x2 * t93 * t56)
      t143 = 0.2D1 * t92 * t141
      t149 = 0.1D1 / (0.1D1 - x2 + t90)
      t150 = rrgg2qqbarh11J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3,
     # x4)
      t156 = log(0.4D1 * t13 * t15 * t56 * t93)
      t158 = rrgg2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3,
     # x4)
      t175 = -(0.90D2 * t8 * (t149 * t150 - t156 * t149 * t158) - 0.180D
     #3 * t26 * t7 * t149 * t158) * t31 / 0.1440D4 - t8 * t149 * t158 * 
     #t34 * t31 / 0.8D1
      t176 = FJET(XB1, XB2, s, -t2 * (-x3 + t88 - x2 + t143), 0.0D0, t2 
     #* (0.1D1 - x2 - x3 + t88 + t143), 0.0D0, 0.0D0, t175)
      t182 = rrgg2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, t126, 0.0D0, x3
     #, x4)
      t187 = rrgg2qqbarh11J2(s, XB1, XB2, z, lh, wd, nf, t126, 0.0D0, x3
     #, x4)
      t189 = t84 ** 2
      t194 = log(-0.4D1 * t40 * t15 * t56 * t105 * t189)
      t205 = -t8 * t182 * t34 * t31 / 0.8D1 - (0.90D2 * t8 * (t187 - t19
     #4 * t182) - 0.180D3 * t26 * t7 * t182) * t34 / 0.720D3
      t206 = FJET(XB1, XB2, s, -t2 * t84 * x3, t108, t2 * t84 * t4, -t11
     #6, 0.0D0, t205)
      rrgg2qqbarht1s1e0 = t82 * t81 - t133 * 0.3141592653589793D1 * t7 *
     # t96 * t130 / 0.8D1 + t176 * t175 + t206 * t205

      end function



      doubleprecision function rrgg2qqbarht1s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh11J1
      doubleprecision rrgg2qqbarh11J2
      doubleprecision rrgg2qqbarh11J3
      doubleprecision rrgg2qqbarh11J4
      doubleprecision rrgg2qqbarh11J5
      doubleprecision rrgg2qqbarh11J6
      doubleprecision rrgg2qqbarh11J7
      t2 = s * (-0.1D1 + z)
      t4 = -0.1D1 + x3
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = 0.3141592653589793D1 * t7
      t9 = rrgg2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3
     #, x4)
      t10 = 0.1D1 / x1
      t14 = rrgg2qqbarh11J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x
     #3, x4)
      t19 = x4 * 0.3141592653589793D1
      t20 = Sin(t19)
      t21 = t20 ** 2
      t22 = z ** 2
      t25 = x3 * t4
      t28 = log(-0.4D1 * t21 / t22 * t25)
      t35 = 0.1D1 / x2
      t39 = t8 * t9 * t10 / 0.8D1 + t8 * t14 / 0.16D2 + (-0.180D3 * 0.31
     #41592653589793D1 * lh - 0.90D2 * t28 * 0.3141592653589793D1) * t7 
     #* t9 / 0.1440D4 + t8 * t9 * t35 / 0.16D2
      t40 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t3
     #9)
      t42 = -0.1D1 + x1
      t52 = rrgg2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, -t42, 0.0D0, x3,
     # x4)
      t56 = FJET(XB1, XB2, s, -t2 * t42 * x3, t2 * x1 * x3, t2 * t42 * t
     #4, -t2 * x1 * t4, 0.0D0, -t8 * t52 * t10 / 0.8D1)
      t63 = 0.2D1 * x2 * x3
      t64 = cos(t19)
      t68 = Sqrt(x2 * (-0.1D1 + x2) * t25)
      t70 = 0.2D1 * t64 * t68
      t78 = rrgg2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 
     #x4)
      t80 = 0.1D1 / (0.1D1 - x2 + x2 * z) * t78 * t35
      t83 = FJET(XB1, XB2, s, -t2 * (-x3 + t63 - x2 + t70), 0.0D0, t2 * 
     #(0.1D1 - x2 - x3 + t63 + t70), 0.0D0, 0.0D0, -t8 * t80 / 0.16D2)
      rrgg2qqbarht1s1em1 = t40 * t39 - t56 * 0.3141592653589793D1 * t7 *
     # t52 * t10 / 0.8D1 - t83 * 0.3141592653589793D1 * t7 * t80 / 0.16D
     #2

      end function



      doubleprecision function rrgg2qqbarht1s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh11J1
      doubleprecision rrgg2qqbarh11J2
      doubleprecision rrgg2qqbarh11J3
      doubleprecision rrgg2qqbarh11J4
      doubleprecision rrgg2qqbarh11J5
      doubleprecision rrgg2qqbarh11J6
      doubleprecision rrgg2qqbarh11J7
      t2 = s * (-0.1D1 + z)
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t9 = rrgg2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3
     #, x4)
      t12 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * (-0.1D1 + x3), 0.0D0
     #, 0.0D0, 0.3141592653589793D1 * t7 * t9 / 0.16D2)
      rrgg2qqbarht1s1em2 = t12 * 0.3141592653589793D1 * t7 * t9 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarht1s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh11J1
      doubleprecision rrgg2qqbarh11J2
      doubleprecision rrgg2qqbarh11J3
      doubleprecision rrgg2qqbarh11J4
      doubleprecision rrgg2qqbarh11J5
      doubleprecision rrgg2qqbarh11J6
      doubleprecision rrgg2qqbarh11J7
      rrgg2qqbarht1s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarht1s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh11J1
      doubleprecision rrgg2qqbarh11J2
      doubleprecision rrgg2qqbarh11J3
      doubleprecision rrgg2qqbarh11J4
      doubleprecision rrgg2qqbarh11J5
      doubleprecision rrgg2qqbarh11J6
      doubleprecision rrgg2qqbarh11J7
      rrgg2qqbarht1s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2qqbarht1s2e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh11J1
      doubleprecision rrgg2qqbarh11J2
      doubleprecision rrgg2qqbarh11J3
      doubleprecision rrgg2qqbarh11J4
      doubleprecision rrgg2qqbarh11J5
      doubleprecision rrgg2qqbarh11J6
      doubleprecision rrgg2qqbarh11J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = 0.3141592653589793D1 * t7
      t9 = rrgg2qqbarh11J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4
     #)
      t10 = z ** 2
      t12 = 0.1D1 / t10 / z
      t13 = x2 * t12
      t14 = x4 * 0.3141592653589793D1
      t15 = Sin(t14)
      t16 = t15 ** 2
      t18 = x3 * t4
      t19 = -0.1D1 + x2
      t23 = log(0.4D1 * t13 * t16 * t18 * t19)
      t24 = rrgg2qqbarh11J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x
     #4)
      t26 = t23 ** 2
      t27 = rrgg2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x
     #4)
      t30 = rrgg2qqbarh11J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3
     #, x4)
      t35 = log(-0.4D1 * t13 * t16 * x3 * t4)
      t36 = rrgg2qqbarh11J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3
     #, x4)
      t38 = t35 ** 2
      t39 = rrgg2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3
     #, x4)
      t45 = 0.3141592653589793D1 * lh
      t52 = lh ** 2
      t54 = 0.3141592653589793D1 ** 2
      t56 = 0.180D3 * t52 - 0.30D2 * t54
      t57 = t56 * 0.3141592653589793D1
      t59 = t7 * (t27 - t39)
      t62 = 0.1D1 / x2
      t65 = rrgg2qqbarh11J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3
     #, x4)
      t71 = log(-0.4D1 * t12 * t16 * t18)
      t72 = t71 ** 2
      t73 = t72 * 0.3141592653589793D1
      t85 = t71 * 0.3141592653589793D1
      t104 = x1 ** 2
      t105 = x2 * t104
      t106 = t105 * t16
      t107 = t12 * x3
      t108 = t107 * t4
      t111 = log(-0.4D1 * t106 * t108)
      t117 = log(0.4D1 * t106 * t107 * t4 * t19)
      t125 = 0.1D1 / x1
      t129 = t104 * t16
      t132 = log(-0.4D1 * t129 * t108)
      t134 = t132 ** 2
      t150 = -(0.90D2 * t8 * (t9 - t23 * t24 + t26 * t27 / 0.2D1 - t30 +
     # t35 * t36 - t38 * t39 / 0.2D1) - 0.180D3 * t45 * t7 * (t24 - t23 
     #* t27 - t36 + t35 * t39) + t57 * t59) * t62 / 0.1440D4 + t8 * t65 
     #/ 0.16D2 + (-0.90D2 * t73 * lh + 0.3141592653589793D1 * (0.60D2 * 
     #lh * t54 - 0.2884936567583026D3 - 0.120D3 * t52 * lh) - 0.15D2 * t
     #72 * t71 * 0.3141592653589793D1 - t85 * t56) * t7 * t39 / 0.1440D4
     # + (-0.180D3 * t45 - 0.90D2 * t85) * t7 * t30 / 0.1440D4 + (0.180D
     #3 * t85 * lh + 0.45D2 * t73 + t57) * t7 * t36 / 0.1440D4 - (0.90D2
     # * t8 * (-t36 + t111 * t39 + t24 - t117 * t27) - 0.180D3 * t45 * t
     #59) * t125 * t62 / 0.720D3 + (0.90D2 * t8 * (t30 - t132 * t36 + t1
     #34 * t39 / 0.2D1) - 0.180D3 * t45 * t7 * (t36 - t132 * t39) + t57 
     #* t7 * t39) * t125 / 0.720D3
      t151 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t
     #150)
      t153 = x3 * x1
      t155 = -0.1D1 + x1
      t157 = t2 * t155 * x3
      t161 = t2 * t155 * t4
      t162 = rrgg2qqbarh11J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, 
     #x4)
      t163 = 0.1D1 / t10
      t165 = t105 * t16 * t163
      t166 = x1 * z
      t167 = -z - x1 + t166
      t168 = 0.1D1 / t167
      t169 = t155 ** 2
      t170 = t168 * t169
      t171 = t18 * t170
      t174 = log(0.4D1 * t165 * t171)
      t175 = rrgg2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, 
     #x4)
      t180 = t7 * t175
      t186 = rrgg2qqbarh11J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, 
     #x4)
      t190 = log(0.4D1 * t129 * t163 * t171)
      t192 = t190 ** 2
      t207 = -(0.90D2 * t8 * (t162 - t174 * t175) - 0.180D3 * t45 * t180
     #) * t125 * t62 / 0.720D3 + (-0.90D2 * t8 * (t186 - t190 * t162 + t
     #192 * t175 / 0.2D1) + 0.180D3 * t45 * t7 * (t162 - t190 * t175) - 
     #t57 * t180) * t125 / 0.720D3
      t208 = FJET(XB1, XB2, s, t2 * t153, -t157, -t2 * x1 * t4, t161, 0.
     #0D0, t207)
      t210 = x3 * z
      t211 = t153 * z
      t212 = x2 * x3
      t213 = t212 * z
      t214 = t153 * x2
      t215 = x2 * z
      t216 = t153 * t215
      t217 = cos(t14)
      t222 = Sqrt(-x3 * t19 * t167 * x2 * t4)
      t224 = 0.2D1 * t217 * t222
      t229 = x1 * x2
      t230 = t229 * z
      t231 = z + x1 - t166 - t215 - t229 + t230 - t210 - t153 + t211 + t
     #213 + t214 - t216 + t212 + t224
      t235 = t1 ** 2
      t242 = 0.1D1 / (z + x1 - t166 - t229 + t230)
      t243 = t167 * t242
      t244 = rrgg2qqbarh11J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t250 = log(-0.4D1 * t165 * t18 * t170 * t19)
      t252 = rrgg2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t262 = 0.90D2 * t8 * (t243 * t244 - t250 * t167 * t242 * t252) - 0
     #.180D3 * t45 * t7 * t243 * t252
      t266 = FJET(XB1, XB2, s, t2 * x1 * (-t210 - t153 + t211 + t213 + t
     #214 - t216 - x2 + t212 + t224) * t168, -t157, -t2 * x1 * t231 * t1
     #68, t161, s * t235 * x2 * x1 * t155 * t168, -t262 * t125 * t62 / 0
     #.720D3)
      rrgg2qqbarht1s2e1 = t151 * t150 + t208 * t207 - t266 * t262 * t125
     # * t62 / 0.720D3

      end function



      doubleprecision function rrgg2qqbarht1s2e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh11J1
      doubleprecision rrgg2qqbarh11J2
      doubleprecision rrgg2qqbarh11J3
      doubleprecision rrgg2qqbarh11J4
      doubleprecision rrgg2qqbarh11J5
      doubleprecision rrgg2qqbarh11J6
      doubleprecision rrgg2qqbarh11J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = 0.3141592653589793D1 * t7
      t9 = rrgg2qqbarh11J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4
     #)
      t10 = z ** 2
      t12 = 0.1D1 / t10 / z
      t13 = x2 * t12
      t14 = x4 * 0.3141592653589793D1
      t15 = Sin(t14)
      t16 = t15 ** 2
      t18 = x3 * t4
      t19 = -0.1D1 + x2
      t23 = log(0.4D1 * t13 * t16 * t18 * t19)
      t24 = rrgg2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x
     #4)
      t26 = rrgg2qqbarh11J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3
     #, x4)
      t31 = log(-0.4D1 * t13 * t16 * x3 * t4)
      t32 = rrgg2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3
     #, x4)
      t37 = 0.3141592653589793D1 * lh
      t38 = t24 - t32
      t43 = 0.1D1 / x2
      t46 = 0.1D1 / x1
      t51 = x1 ** 2
      t52 = t51 * t16
      t57 = log(-0.4D1 * t52 * t12 * x3 * t4)
      t68 = rrgg2qqbarh11J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3
     #, x4)
      t75 = log(-0.4D1 * t12 * t16 * t18)
      t76 = t75 * 0.3141592653589793D1
      t84 = t75 ** 2
      t87 = lh ** 2
      t89 = 0.3141592653589793D1 ** 2
      t97 = -(0.90D2 * t8 * (t9 - t23 * t24 - t26 + t31 * t32) - 0.180D3
     # * t37 * t7 * t38) * t43 / 0.1440D4 - t8 * t38 * t46 * t43 / 0.8D1
     # + (0.90D2 * t8 * (t26 - t57 * t32) - 0.180D3 * t37 * t7 * t32) * 
     #t46 / 0.720D3 + t8 * t68 / 0.16D2 + (-0.180D3 * t37 - 0.90D2 * t76
     #) * t7 * t26 / 0.1440D4 + (0.180D3 * t76 * lh + 0.45D2 * t84 * 0.3
     #141592653589793D1 + 0.3141592653589793D1 * (0.180D3 * t87 - 0.30D2
     # * t89)) * t7 * t32 / 0.1440D4
      t98 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t9
     #7)
      t100 = x3 * x1
      t102 = -0.1D1 + x1
      t104 = t2 * t102 * x3
      t108 = t2 * t102 * t4
      t109 = rrgg2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, 
     #x4)
      t114 = rrgg2qqbarh11J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, 
     #x4)
      t117 = x1 * z
      t118 = -z - x1 + t117
      t119 = 0.1D1 / t118
      t120 = t102 ** 2
      t125 = log(0.4D1 * t52 / t10 * t18 * t119 * t120)
      t136 = -t8 * t109 * t46 * t43 / 0.8D1 + (-0.90D2 * t8 * (t114 - t1
     #25 * t109) + 0.180D3 * t37 * t7 * t109) * t46 / 0.720D3
      t137 = FJET(XB1, XB2, s, t2 * t100, -t104, -t2 * x1 * t4, t108, 0.
     #0D0, t136)
      t139 = x3 * z
      t140 = t100 * z
      t141 = x2 * x3
      t142 = t141 * z
      t143 = t100 * x2
      t144 = x2 * z
      t145 = t100 * t144
      t146 = cos(t14)
      t151 = Sqrt(-x3 * t19 * t118 * x2 * t4)
      t153 = 0.2D1 * t146 * t151
      t158 = x1 * x2
      t159 = t158 * z
      t160 = z + x1 - t117 - t144 - t158 + t159 - t139 - t100 + t140 + t
     #142 + t143 - t145 + t141 + t153
      t164 = t1 ** 2
      t173 = rrgg2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t176 = 0.1D1 / (z + x1 - t117 - t158 + t159) * t173 * t46 * t43
      t179 = FJET(XB1, XB2, s, t2 * x1 * (-t139 - t100 + t140 + t142 + t
     #143 - t145 - x2 + t141 + t153) * t119, -t104, -t2 * x1 * t160 * t1
     #19, t108, s * t164 * x2 * x1 * t102 * t119, -t8 * t118 * t176 / 0.
     #8D1)
      rrgg2qqbarht1s2e0 = t98 * t97 + t137 * t136 - t179 * 0.31415926535
     #89793D1 * t7 * t118 * t176 / 0.8D1

      end function



      doubleprecision function rrgg2qqbarht1s2em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh11J1
      doubleprecision rrgg2qqbarh11J2
      doubleprecision rrgg2qqbarh11J3
      doubleprecision rrgg2qqbarh11J4
      doubleprecision rrgg2qqbarh11J5
      doubleprecision rrgg2qqbarh11J6
      doubleprecision rrgg2qqbarh11J7
      t2 = s * (-0.1D1 + z)
      t4 = -0.1D1 + x3
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = 0.3141592653589793D1 * t7
      t9 = rrgg2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3,
     # x4)
      t10 = 0.1D1 / x1
      t14 = rrgg2qqbarh11J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3
     #, x4)
      t19 = z ** 2
      t23 = Sin(x4 * 0.3141592653589793D1)
      t24 = t23 ** 2
      t29 = log(-0.4D1 / t19 / z * t24 * x3 * t4)
      t36 = rrgg2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x
     #4)
      t42 = t8 * t9 * t10 / 0.8D1 + t8 * t14 / 0.16D2 + (-0.180D3 * 0.31
     #41592653589793D1 * lh - 0.90D2 * t29 * 0.3141592653589793D1) * t7 
     #* t9 / 0.1440D4 - t8 * (t36 - t9) / x2 / 0.16D2
      t43 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t4
     #2)
      t47 = -0.1D1 + x1
      t54 = rrgg2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x
     #4)
      t58 = FJET(XB1, XB2, s, t2 * x1 * x3, -t2 * t47 * x3, -t2 * x1 * t
     #4, t2 * t47 * t4, 0.0D0, -t8 * t54 * t10 / 0.8D1)
      rrgg2qqbarht1s2em1 = t43 * t42 - t58 * 0.3141592653589793D1 * t7 *
     # t54 * t10 / 0.8D1

      end function



      doubleprecision function rrgg2qqbarht1s2em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh11J1
      doubleprecision rrgg2qqbarh11J2
      doubleprecision rrgg2qqbarh11J3
      doubleprecision rrgg2qqbarh11J4
      doubleprecision rrgg2qqbarh11J5
      doubleprecision rrgg2qqbarh11J6
      doubleprecision rrgg2qqbarh11J7
      t2 = s * (-0.1D1 + z)
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t9 = rrgg2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3,
     # x4)
      t12 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * (-0.1D1 + x3)
     #, 0.0D0, 0.3141592653589793D1 * t7 * t9 / 0.16D2)
      rrgg2qqbarht1s2em2 = t12 * 0.3141592653589793D1 * t7 * t9 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarht1s2em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh11J1
      doubleprecision rrgg2qqbarh11J2
      doubleprecision rrgg2qqbarh11J3
      doubleprecision rrgg2qqbarh11J4
      doubleprecision rrgg2qqbarh11J5
      doubleprecision rrgg2qqbarh11J6
      doubleprecision rrgg2qqbarh11J7
      rrgg2qqbarht1s2em3 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarht1s2em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh11J1
      doubleprecision rrgg2qqbarh11J2
      doubleprecision rrgg2qqbarh11J3
      doubleprecision rrgg2qqbarh11J4
      doubleprecision rrgg2qqbarh11J5
      doubleprecision rrgg2qqbarh11J6
      doubleprecision rrgg2qqbarh11J7
      rrgg2qqbarht1s2em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgg2qqbarh11J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = 0.1D1 - z
      t4 = t2 * t3
      t5 = z ** 2
      t9 = 0.1D1 - x1
      t10 = 0.1D1 - x2
      t13 = z + x1 * t10 * t3
      t14 = t9 * t13
      t16 = z + x1 * t3
      t17 = 0.1D1 / t16
      t21 = t3 ** 2
      t22 = t21 * t3
      t23 = t2 * t22
      t24 = t9 ** 2
      t25 = x1 * t24
      t26 = x3 ** 2
      t27 = t25 * t26
      t30 = 0.1D1 - x3
      t31 = t30 ** 2
      t32 = t25 * t31
      t35 = t2 * z
      t36 = t21 * t24
      t43 = t2 * t21
      t44 = x1 ** 2
      t45 = t44 * t17
      t46 = x3 * t10
      t50 = cos(x4 * 0.3141592653589793D1)
      t54 = Sqrt(t46 * t16 * x2 * t30)
      t56 = 0.2D1 * t50 * t54
      t57 = t46 * t16 + x2 * t30 - t56
      t64 = t30 * t10 * t16 + x2 * x3 + t56
      t68 = t21 ** 2
      t70 = t2 * t68 * t3
      t71 = t44 * x1
      t73 = x2 ** 2
      t75 = t16 ** 2
      t76 = 0.1D1 / t75
      t80 = t23 * t44
      t81 = x2 * t9
      t86 = t13 * t17
      t90 = t43 * t9
      t94 = t43 * x2
      t95 = x1 * t9
      t99 = t2 * t68
      t100 = t99 * t73
      t101 = t44 * t24
      t106 = t2 * t68 * t21
      t109 = t24 * t9
      t110 = t71 * t109
      t112 = 0.1D1 / t75 / t16
      t116 = -0.1728D4 * t4 * x1 * t5 + 0.1440D4 * t4 * t14 * t17 - 0.28
     #8D3 * t23 * t27 - 0.288D3 * t23 * t32 - 0.576D3 * t35 * t36 * t31 
     #- 0.576D3 * t35 * t36 * t26 - 0.1440D4 * t43 * t45 * t57 - 0.1440D
     #4 * t43 * t45 * t64 + 0.3466D4 * t70 * t71 * t73 * t24 * t76 - 0.1
     #632D4 * t80 * t81 * t17 - 0.1728D4 * t4 * t9 * t86 * t5 - 0.2304D4
     # * t90 * t86 * x1 + 0.224D3 * t94 * t95 * t17 - 0.128D3 * t100 * t
     #101 * t76 - 0.576D3 * t106 * t73 * x2 * t110 * t112
      t117 = t35 * t22
      t124 = t24 * x3 * t30
      t127 = t35 * t21
      t130 = t43 * t24
      t137 = t71 * t76
      t138 = t57 ** 2
      t142 = t64 ** 2
      t146 = t43 * t44
      t147 = z * t17
      t154 = t44 * t76
      t162 = t13 * t76
      t163 = t44 * x2
      t168 = t13 * t112
      t169 = t73 * t44
      t174 = x1 * x2
      t178 = x1 * z
      t182 = 0.144D3 * t117 * t27 + 0.144D3 * t117 * t32 + 0.576D3 * t23
     # * x1 * t124 + 0.1152D4 * t127 * t124 - 0.1440D4 * t130 * t86 * t3
     #0 - 0.1440D4 * t130 * t86 * x3 + 0.288D3 * t117 * t137 * t138 + 0.
     #288D3 * t117 * t137 * t142 - 0.1728D4 * t146 * t147 * t57 - 0.1728
     #D4 * t146 * t147 * t64 - 0.576D3 * t127 * t154 * t138 - 0.576D3 * 
     #t127 * t154 * t142 + 0.7896D4 * t99 * t24 * t162 * t163 + 0.3466D4
     # * t70 * t109 * t168 * t169 - 0.1632D4 * t23 * t24 * t162 * t174 -
     # 0.3456D4 * t90 * t86 * t178
      t195 = x3 * t30
      t199 = t70 * x2
      t200 = t44 * t109
      t201 = t17 * t26
      t205 = t99 * x1
      t206 = t109 * t13
      t207 = t206 * t201
      t210 = t17 * t31
      t214 = t206 * t210
      t221 = t99 * t44
      t222 = x2 * t24
      t227 = t99 * x2
      t228 = x1 * t109
      t243 = -0.1728D4 * t80 * t81 * t147 - 0.1728D4 * t94 * t95 * t17 *
     # t5 - 0.1728D4 * t100 * t101 * t76 * z - 0.288D3 * t117 * t25 * t1
     #95 - 0.144D3 * t199 * t200 * t201 - 0.432D3 * t205 * t207 - 0.144D
     #3 * t199 * t200 * t210 - 0.432D3 * t205 * t214 + 0.288D3 * t117 * 
     #t207 + 0.288D3 * t117 * t214 - 0.3523D4 * t221 * t222 * t17 * x3 -
     # 0.576D3 * t227 * t228 * t201 - 0.3523D4 * t221 * t222 * t17 * t30
     # - 0.576D3 * t227 * t228 * t210 - 0.1728D4 * t130 * t86 * z * x3
      t248 = t106 * t73
      t249 = t76 * t30
      t253 = t76 * x3
      t257 = t44 ** 2
      t258 = t257 * t24
      t267 = t99 * t71
      t268 = t112 * t138
      t272 = t112 * t142
      t280 = t71 * t9
      t284 = t17 * t57
      t285 = t9 * t30
      t286 = t284 * t285
      t289 = t23 * t9
      t291 = t168 * t44 * t138
      t294 = t9 * x3
      t295 = t294 * t284
      t305 = t17 * t64
      t306 = t285 * t305
      t310 = t168 * t44 * t142
      t313 = -0.1728D4 * t130 * t86 * z * t30 + 0.144D3 * t248 * t110 * 
     #t249 + 0.144D3 * t248 * t110 * t253 + 0.14832D5 * t248 * t258 * t1
     #12 * t57 + 0.14832D5 * t248 * t258 * t112 * t64 - 0.432D3 * t267 *
     # t14 * t268 - 0.432D3 * t267 * t14 * t272 + 0.1244D4 * t267 * t81 
     #* t76 * t57 - 0.576D3 * t227 * t280 * t268 + 0.144D3 * t80 * t286 
     #- 0.288D3 * t289 * t291 - 0.144D3 * t80 * t295 + 0.1244D4 * t267 *
     # t81 * t76 * t64 - 0.576D3 * t227 * t280 * t272 - 0.144D3 * t80 * 
     #t306 - 0.288D3 * t289 * t310
      t316 = t305 * t294
      t319 = t57 * t64
      t328 = t24 * t13
      t329 = t23 * t328
      t330 = t76 * x2
      t334 = t70 * t163
      t335 = t109 * t76
      t336 = t13 * x3
      t340 = t106 * t169
      t341 = t24 ** 2
      t342 = t341 * t112
      t346 = t13 * t30
      t358 = t109 * t17 * t195
      t367 = t99 * t206
      t368 = x3 * x1
      t372 = x1 * t30
      t379 = 0.144D3 * t80 * t316 + 0.1152D4 * t127 * t154 * t319 - 0.57
     #6D3 * t2 * t5 * z + 0.1440D4 * t4 * x1 - 0.1728D4 * t329 * t330 * 
     #t178 + 0.14976D5 * t334 * t335 * t336 + 0.14832D5 * t340 * t342 * 
     #t336 + 0.14976D5 * t334 * t335 * t346 + 0.14832D5 * t340 * t342 * 
     #t346 - 0.864D3 * t99 * t228 * t86 * t195 - 0.288D3 * t334 * t358 -
     # 0.576D3 * t70 * t174 * t341 * t76 * t336 * t30 + 0.1244D4 * t367 
     #* t330 * t368 + 0.1244D4 * t367 * t330 * t372 + 0.1152D4 * t99 * t
     #174 * t358
      t380 = x2 * t71
      t381 = t70 * t380
      t382 = t24 * t112
      t383 = t13 * t57
      t387 = t75 ** 2
      t388 = 0.1D1 / t387
      t389 = t24 * t388
      t395 = t106 * t73 * t71
      t396 = t109 * t388
      t400 = t99 * t101
      t401 = x3 * t57
      t405 = t24 * t76
      t406 = t405 * t401
      t409 = t109 * t112
      t414 = t57 * t30
      t418 = t405 * t414
      t425 = t13 * t64
      t436 = t64 * x3
      t440 = t405 * t436
      t447 = t30 * t64
      t451 = 0.14976D5 * t381 * t382 * t383 - 0.144D3 * t381 * t389 * t1
     #3 * t138 + 0.144D3 * t395 * t396 * t383 - 0.864D3 * t400 * t162 * 
     #t401 - 0.14832D5 * t381 * t406 - 0.14832D5 * t334 * t409 * t336 * 
     #t57 - 0.1440D4 * t400 * t162 * t414 - 0.14976D5 * t381 * t418 - 0.
     #14976D5 * t334 * t409 * t383 * t30 + 0.14976D5 * t381 * t382 * t42
     #5 - 0.144D3 * t381 * t389 * t13 * t142 + 0.144D3 * t395 * t396 * t
     #425 - 0.1440D4 * t400 * t162 * t436 - 0.14976D5 * t381 * t440 - 0.
     #14976D5 * t334 * t409 * t425 * x3 - 0.864D3 * t400 * t162 * t447
      t453 = t405 * t447
      t467 = t9 * t112 * t319
      t475 = t35 * t22 * t24
      t480 = x1 * t57
      t486 = t35 * t22 * t44
      t490 = t35 * t22 * t9
      t495 = x1 * t64
      t504 = t44 * t57
      t515 = -0.14832D5 * t381 * t453 - 0.14832D5 * t334 * t409 * t346 *
     # t64 - 0.864D3 * t99 * t280 * t168 * t319 - 0.576D3 * t70 * x2 * t
     #257 * t467 - 0.288D3 * t381 * t389 * t383 * t64 + 0.288D3 * t475 *
     # t162 * t368 * t57 - 0.288D3 * t475 * t162 * t480 * t30 - 0.288D3 
     #* t486 * t286 + 0.144D3 * t490 * t291 + 0.288D3 * t486 * t295 - 0.
     #288D3 * t475 * t162 * t495 * x3 + 0.288D3 * t475 * t162 * t372 * t
     #64 - 0.288D3 * t490 * t168 * t504 * t64 + 0.288D3 * t486 * t306 + 
     #0.144D3 * t490 * t310 - 0.288D3 * t486 * t316
      t516 = t99 * t328
      t517 = t112 * x2
      t524 = t99 * t163
      t527 = t76 * x1
      t556 = t35 * t21 * x1
      t560 = t35 * t21 * t9
      t569 = -0.3523D4 * t516 * t517 * t504 - 0.144D3 * t329 * t253 * t4
     #80 - 0.864D3 * t524 * t406 + 0.144D3 * t329 * t527 * t414 + 0.864D
     #3 * t524 * t418 - 0.3523D4 * t516 * t517 * t44 * t64 + 0.144D3 * t
     #329 * t527 * t436 + 0.864D3 * t524 * t440 - 0.144D3 * t329 * t249 
     #* t495 - 0.864D3 * t524 * t453 + 0.576D3 * t23 * t14 * t112 * t44 
     #* t319 + 0.1152D4 * t99 * t380 * t467 + 0.1152D4 * t556 * t286 - 0
     #.1152D4 * t560 * t368 * t284 - 0.1152D4 * t560 * t372 * t305 + 0.1
     #152D4 * t556 * t316
      rrgg2qqbarh11J1 = -wd * (t116 + t182 + t243 + t313 + t379 + t451 +
     # t515 + t569) * nf / t1 / z / 0.3141592653589793D1 / 0.96D2

      end function
  
   
 

      doubleprecision function rrgg2qqbarh11J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = 0.1D1 - z
      t4 = t2 * t3
      t5 = z ** 2
      t8 = 0.1728D4 * t4 * x1 * t5
      t9 = t3 ** 2
      t10 = t9 ** 2
      t12 = t2 * t10 * t3
      t13 = x1 ** 2
      t14 = t13 * x1
      t16 = x2 ** 2
      t17 = 0.1D1 - x1
      t18 = t17 ** 2
      t21 = z + x1 * t3
      t22 = t21 ** 2
      t23 = 0.1D1 / t22
      t25 = t12 * t14 * t16 * t18 * t23
      t27 = t9 * t3
      t28 = t2 * t27
      t29 = t28 * t13
      t30 = x2 * t17
      t31 = 0.1D1 / t21
      t33 = t29 * t30 * t31
      t36 = 0.1D1 - x2
      t39 = z + x1 * t36 * t3
      t40 = t39 * t31
      t43 = 0.1728D4 * t4 * t17 * t40 * t5
      t44 = t2 * t9
      t45 = t44 * t17
      t47 = t45 * t40 * x1
      t49 = t44 * x2
      t50 = x1 * t17
      t52 = t49 * t50 * t31
      t54 = t2 * t10
      t55 = t54 * t16
      t56 = t13 * t18
      t58 = t55 * t56 * t23
      t61 = t2 * t10 * t9
      t62 = t16 * x2
      t64 = t18 * t17
      t65 = t14 * t64
      t67 = 0.1D1 / t22 / t21
      t69 = t61 * t62 * t65 * t67
      t71 = t2 * z
      t72 = t71 * t27
      t73 = x1 * t18
      t74 = x3 ** 2
      t75 = t73 * t74
      t76 = t72 * t75
      t78 = 0.1D1 - x3
      t79 = t78 ** 2
      t80 = t73 * t79
      t81 = t72 * t80
      t85 = t18 * x3 * t78
      t88 = t71 * t9
      t91 = t44 * t18
      t93 = t91 * t40 * t78
      t96 = t91 * t40 * x3
      t98 = t14 * t23
      t99 = x3 * t36
      t103 = cos(x4 * 0.3141592653589793D1)
      t107 = Sqrt(t99 * t21 * x2 * t78)
      t109 = 0.2D1 * t103 * t107
      t110 = t99 * t21 + x2 * t78 - t109
      t111 = t110 ** 2
      t112 = t98 * t111
      t114 = 0.288D3 * t72 * t112
      t115 = -t8 + 0.3466D4 * t25 - 0.1632D4 * t33 - t43 - 0.2304D4 * t4
     #7 + 0.224D3 * t52 - 0.128D3 * t58 - 0.576D3 * t69 + 0.144D3 * t76 
     #+ 0.144D3 * t81 + 0.576D3 * t28 * x1 * t85 + 0.1152D4 * t88 * t85 
     #- 0.1440D4 * t93 - 0.1440D4 * t96 + t114
      t119 = t78 * t36 * t21 + x2 * x3 + t109
      t120 = t119 ** 2
      t121 = t98 * t120
      t123 = 0.288D3 * t72 * t121
      t124 = t44 * t13
      t125 = z * t31
      t128 = 0.1728D4 * t124 * t125 * t110
      t131 = 0.1728D4 * t124 * t125 * t119
      t132 = t13 * t23
      t140 = t39 * t23
      t141 = t13 * x2
      t143 = t54 * t18 * t140 * t141
      t146 = t39 * t67
      t147 = t16 * t13
      t149 = t12 * t64 * t146 * t147
      t151 = t28 * t18
      t152 = x1 * x2
      t154 = t151 * t140 * t152
      t156 = x1 * z
      t159 = 0.3456D4 * t45 * t40 * t156
      t162 = 0.1728D4 * t29 * t30 * t125
      t166 = 0.1728D4 * t49 * t50 * t31 * t5
      t170 = 0.1728D4 * t55 * t56 * t23 * z
      t171 = x3 * t78
      t173 = t72 * t73 * t171
      t175 = x2 * t12
      t176 = t13 * t64
      t177 = t31 * t74
      t179 = t175 * t176 * t177
      t181 = t54 * x1
      t182 = t64 * t39
      t183 = t182 * t177
      t184 = t181 * t183
      t186 = t31 * t79
      t188 = t175 * t176 * t186
      t190 = t123 - t128 - t131 - 0.576D3 * t88 * t132 * t111 - 0.576D3 
     #* t88 * t132 * t120 + 0.7896D4 * t143 + 0.3466D4 * t149 - 0.1632D4
     # * t154 - t159 - t162 - t166 - t170 - 0.288D3 * t173 - 0.144D3 * t
     #179 - 0.432D3 * t184 - 0.144D3 * t188
      t192 = t182 * t186
      t193 = t181 * t192
      t196 = 0.288D3 * t72 * t183
      t198 = 0.288D3 * t72 * t192
      t199 = t54 * t13
      t200 = x2 * t18
      t203 = t199 * t200 * t31 * x3
      t205 = t54 * x2
      t206 = x1 * t64
      t212 = t199 * t200 * t31 * t78
      t220 = 0.1728D4 * t91 * t40 * z * x3
      t224 = 0.1728D4 * t91 * t40 * z * t78
      t225 = t61 * t16
      t226 = t23 * t78
      t228 = t225 * t65 * t226
      t230 = t23 * x3
      t232 = t225 * t65 * t230
      t234 = t13 ** 2
      t235 = t234 * t18
      t238 = t225 * t235 * t67 * t110
      t242 = t225 * t235 * t67 * t119
      t244 = t54 * t14
      t245 = t17 * t39
      t246 = t67 * t111
      t248 = t244 * t245 * t246
      t250 = t67 * t120
      t252 = t244 * t245 * t250
      t254 = -0.432D3 * t193 + t196 + t198 - 0.3523D4 * t203 - 0.576D3 *
     # t205 * t206 * t177 - 0.3523D4 * t212 - 0.576D3 * t205 * t206 * t1
     #86 - t220 - t224 + 0.144D3 * t228 + 0.144D3 * t232 + 0.14832D5 * t
     #238 + 0.14832D5 * t242 - 0.432D3 * t248 - 0.432D3 * t252
      t255 = t23 * t110
      t257 = t244 * t30 * t255
      t259 = t14 * t17
      t263 = t31 * t110
      t264 = t17 * t78
      t265 = t263 * t264
      t266 = t29 * t265
      t268 = t28 * t17
      t270 = t146 * t13 * t111
      t273 = t17 * x3
      t274 = t273 * t263
      t275 = t29 * t274
      t279 = t244 * t30 * t23 * t119
      t284 = t31 * t119
      t285 = t284 * t264
      t286 = t29 * t285
      t289 = t146 * t13 * t120
      t292 = t284 * t273
      t293 = t29 * t292
      t295 = t110 * t119
      t301 = 0.1440D4 * t4 * t245 * t31
      t306 = t9 * t18
      t313 = 0.1244D4 * t257 - 0.576D3 * t205 * t259 * t246 + 0.144D3 * 
     #t266 - 0.288D3 * t268 * t270 - 0.144D3 * t275 + 0.1244D4 * t279 - 
     #0.576D3 * t205 * t259 * t250 - 0.144D3 * t286 - 0.288D3 * t268 * t
     #289 + 0.144D3 * t293 + 0.1152D4 * t88 * t132 * t295 + t301 - 0.288
     #D3 * t28 * t75 - 0.288D3 * t28 * t80 - 0.576D3 * t71 * t306 * t79 
     #- 0.576D3 * t71 * t306 * t74
      t316 = t13 * t31
      t318 = t44 * t316 * t110
      t321 = t44 * t316 * t119
      t324 = 0.1440D4 * t4 * x1
      t327 = 0.576D3 * t2 * t5 * z
      t328 = t18 * t39
      t329 = t28 * t328
      t330 = t23 * x2
      t333 = 0.1728D4 * t329 * t330 * t156
      t334 = t12 * t141
      t335 = t64 * t23
      t336 = t39 * x3
      t338 = t334 * t335 * t336
      t340 = t61 * t147
      t341 = t18 ** 2
      t342 = t341 * t67
      t344 = t340 * t342 * t336
      t346 = t39 * t78
      t348 = t334 * t335 * t346
      t351 = t340 * t342 * t346
      t354 = t40 * t171
      t355 = t54 * t206 * t354
      t358 = t64 * t31 * t171
      t359 = t334 * t358
      t361 = t12 * t152
      t362 = t341 * t23
      t365 = t361 * t362 * t336 * t78
      t367 = t54 * t182
      t368 = x3 * x1
      t370 = t367 * t330 * t368
      t372 = x1 * t78
      t374 = t367 * t330 * t372
      t379 = -0.1440D4 * t318 - 0.1440D4 * t321 + t324 - t327 - t333 + 0
     #.14976D5 * t338 + 0.14832D5 * t344 + 0.14976D5 * t348 + 0.14832D5 
     #* t351 - 0.864D3 * t355 - 0.288D3 * t359 - 0.576D3 * t365 + 0.1244
     #D4 * t370 + 0.1244D4 * t374 + 0.1152D4 * t54 * t152 * t358
      t380 = x2 * t14
      t381 = t12 * t380
      t382 = t18 * t67
      t383 = t39 * t110
      t385 = t381 * t382 * t383
      t387 = t22 ** 2
      t388 = 0.1D1 / t387
      t389 = t18 * t388
      t392 = t381 * t389 * t39 * t111
      t395 = t61 * t16 * t14
      t396 = t64 * t388
      t398 = t395 * t396 * t383
      t400 = t54 * t56
      t401 = x3 * t110
      t403 = t400 * t140 * t401
      t405 = t18 * t23
      t406 = t405 * t401
      t407 = t381 * t406
      t409 = t64 * t67
      t412 = t334 * t409 * t336 * t110
      t414 = t110 * t78
      t416 = t400 * t140 * t414
      t418 = t405 * t414
      t419 = t381 * t418
      t423 = t334 * t409 * t383 * t78
      t425 = t39 * t119
      t427 = t381 * t382 * t425
      t431 = t381 * t389 * t39 * t120
      t434 = t395 * t396 * t425
      t436 = t119 * x3
      t438 = t400 * t140 * t436
      t440 = t405 * t436
      t441 = t381 * t440
      t445 = t334 * t409 * t425 * x3
      t447 = t78 * t119
      t449 = t400 * t140 * t447
      t451 = 0.14976D5 * t385 - 0.144D3 * t392 + 0.144D3 * t398 - 0.864D
     #3 * t403 - 0.14832D5 * t407 - 0.14832D5 * t412 - 0.1440D4 * t416 -
     # 0.14976D5 * t419 - 0.14976D5 * t423 + 0.14976D5 * t427 - 0.144D3 
     #* t431 + 0.144D3 * t434 - 0.1440D4 * t438 - 0.14976D5 * t441 - 0.1
     #4976D5 * t445 - 0.864D3 * t449
      t453 = t405 * t447
      t454 = t381 * t453
      t458 = t334 * t409 * t346 * t119
      t462 = t54 * t259 * t146 * t295
      t467 = t17 * t67 * t295
      t468 = t12 * x2 * t234 * t467
      t472 = t381 * t389 * t383 * t119
      t475 = t71 * t27 * t18
      t479 = 0.288D3 * t475 * t140 * t368 * t110
      t480 = x1 * t110
      t484 = 0.288D3 * t475 * t140 * t480 * t78
      t486 = t71 * t27 * t13
      t488 = 0.288D3 * t486 * t265
      t490 = t71 * t27 * t17
      t491 = t490 * t270
      t494 = 0.288D3 * t486 * t274
      t495 = x1 * t119
      t499 = 0.288D3 * t475 * t140 * t495 * x3
      t503 = 0.288D3 * t475 * t140 * t372 * t119
      t504 = t13 * t110
      t507 = t490 * t146 * t504 * t119
      t510 = 0.288D3 * t486 * t285
      t511 = t490 * t289
      t514 = 0.288D3 * t486 * t292
      t515 = -0.14832D5 * t454 - 0.14832D5 * t458 - 0.864D3 * t462 - 0.5
     #76D3 * t468 - 0.288D3 * t472 + t479 - t484 - t488 + 0.144D3 * t491
     # + t494 - t499 + t503 - 0.288D3 * t507 + t510 + 0.144D3 * t511 - t
     #514
      t516 = t54 * t328
      t517 = t67 * x2
      t519 = t516 * t517 * t504
      t522 = t329 * t230 * t480
      t524 = t54 * t141
      t527 = t23 * x1
      t529 = t329 * t527 * t414
      t533 = t13 * t119
      t535 = t516 * t517 * t533
      t538 = t329 * t527 * t436
      t543 = t329 * t226 * t495
      t556 = t71 * t9 * x1
      t560 = t71 * t9 * t17
      t569 = -0.3523D4 * t519 - 0.144D3 * t522 - 0.864D3 * t524 * t406 +
     # 0.144D3 * t529 + 0.864D3 * t524 * t418 - 0.3523D4 * t535 + 0.144D
     #3 * t538 + 0.864D3 * t524 * t440 - 0.144D3 * t543 - 0.864D3 * t524
     # * t453 + 0.576D3 * t28 * t245 * t67 * t13 * t295 + 0.1152D4 * t54
     # * t380 * t467 + 0.1152D4 * t556 * t265 - 0.1152D4 * t560 * t368 *
     # t263 - 0.1152D4 * t560 * t372 * t284 + 0.1152D4 * t556 * t292
      t585 = t8 + 0.4940D4 * t25 + 0.4816D4 * t33 + t43 + 0.4320D4 * t47
     # - 0.1376D4 * t52 + 0.1024D4 * t58 + 0.352D3 * t69 - 0.288D3 * t76
     # - 0.288D3 * t81 + 0.1728D4 * t93 + 0.1728D4 * t96 - t114 - t123
      t588 = t2 * t10 * t27 * t62
      t593 = t28 * t64
      t609 = t128 + t131 + 0.3096D4 * t588 * t234 * t64 * t67 - 0.288D3 
     #* t593 * t40 * t74 - 0.288D3 * t593 * t40 * t79 - 0.576D3 * t28 * 
     #t14 * t255 * t119 + 0.9344D4 * t143 + 0.4940D4 * t149 + 0.4816D4 *
     # t154 + t159 + t162 + t166 + t170 + 0.576D3 * t173 - 0.936D3 * t17
     #9
      t622 = 0.1008D4 * t184 - 0.936D3 * t188 + 0.1008D4 * t193 - t196 -
     # t198 - 0.5266D4 * t203 - 0.5266D4 * t212 + t220 + t224 - 0.2160D4
     # * t228 - 0.2160D4 * t232 - 0.15552D5 * t238 - 0.15552D5 * t242 + 
     #0.1008D4 * t248 + 0.1008D4 * t252
      t645 = t234 * t17
      t658 = -0.3776D4 * t257 - 0.72D2 * t266 + 0.72D2 * t275 - 0.3776D4
     # * t279 + 0.72D2 * t286 - 0.72D2 * t293 + 0.3096D4 * t588 * t14 * 
     #t341 * t388 * t39 + 0.6192D4 * t225 * t65 * t146 - 0.2808D4 * t151
     # * t40 * t368 - 0.2808D4 * t151 * t40 * t372 - 0.576D3 * t593 * t3
     #54 + 0.432D3 * t175 * t645 * t246 + 0.432D3 * t175 * t645 * t250 -
     # 0.576D3 * t72 * t98 * t295 - 0.2808D4 * t29 * t245 * t255
      t675 = -0.2808D4 * t268 * t140 * t533 - t301 + 0.1728D4 * t318 + 0
     #.1728D4 * t321 - 0.288D3 * t28 * t112 - 0.288D3 * t28 * t121 - t32
     #4 + t327 + t333 - 0.14616D5 * t338 - 0.15552D5 * t344 - 0.14616D5 
     #* t348 - 0.15552D5 * t351 + 0.2016D4 * t355
      t691 = -0.1872D4 * t359 + 0.864D3 * t365 - 0.3776D4 * t370 - 0.377
     #6D4 * t374 - 0.14616D5 * t385 - 0.936D3 * t392 - 0.2160D4 * t398 +
     # 0.2016D4 * t403 + 0.15696D5 * t407 + 0.15696D5 * t412 + 0.1728D4 
     #* t416 + 0.15552D5 * t419 + 0.15552D5 * t423 - 0.14616D5 * t427 - 
     #0.936D3 * t431
      t704 = -0.2160D4 * t434 + 0.1728D4 * t438 + 0.15552D5 * t441 + 0.1
     #5552D5 * t445 + 0.2016D4 * t449 + 0.15696D5 * t454 + 0.15696D5 * t
     #458 + 0.2016D4 * t462 + 0.864D3 * t468 - 0.1872D4 * t472 + t479 - 
     #t484 - t488 - 0.288D3 * t491 + t494
      t725 = -t499 + t503 + 0.576D3 * t507 + t510 - 0.288D3 * t511 - t51
     #4 - 0.5266D4 * t519 + 0.72D2 * t522 - 0.72D2 * t529 - 0.5266D4 * t
     #535 - 0.72D2 * t538 + 0.72D2 * t543 + 0.432D3 * t361 * t362 * t39 
     #* t74 + 0.432D3 * t361 * t362 * t39 * t79 - 0.576D3 * t71 * t27 * 
     #t64 * t354
      rrgg2qqbarh11J2 = -(0.2D1 * wd * (t115 + t190 + t254 + t313 + t379
     # + t451 + t515 + t569) + wd * (t585 + t609 + t622 + t658 + t675 + 
     #t691 + t704 + t725)) * nf / t1 / z / 0.3141592653589793D1 / 0.96D2

      end function
  
   
 

      doubleprecision function rrgg2qqbarh11J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t5 = t4 ** 2
      t6 = t2 * t5
      t7 = x1 ** 2
      t8 = x2 * t7
      t9 = t6 * t8
      t10 = 0.1D1 - x1
      t11 = t10 ** 2
      t13 = z + x1 * t3
      t14 = t13 ** 2
      t15 = 0.1D1 / t14
      t16 = t11 * t15
      t17 = 0.1D1 - x2
      t18 = t17 * x3
      t20 = 0.1D1 - x3
      t23 = cos(x4 * 0.3141592653589793D1)
      t27 = Sqrt(t18 * t13 * x2 * t20)
      t29 = 0.2D1 * t23 * t27
      t30 = t13 * t18 + x2 * t20 - t29
      t31 = x3 * t30
      t32 = t16 * t31
      t35 = t4 * t3
      t36 = t2 * t35
      t39 = z + x1 * t17 * t3
      t40 = t11 * t39
      t41 = t36 * t40
      t42 = t15 * x1
      t43 = t30 * t20
      t45 = t41 * t42 * t43
      t47 = t16 * t43
      t50 = t2 * t3
      t52 = 0.1440D4 * t50 * x1
      t54 = t2 * t5 * t3
      t55 = t54 * t8
      t56 = t11 * t10
      t57 = t56 * t15
      t58 = t39 * x3
      t60 = t55 * t57 * t58
      t63 = t2 * t5 * t4
      t64 = x2 ** 2
      t65 = t64 * t7
      t66 = t63 * t65
      t67 = t11 ** 2
      t69 = 0.1D1 / t14 / t13
      t70 = t67 * t69
      t72 = t66 * t70 * t58
      t74 = t39 * t20
      t76 = t55 * t57 * t74
      t79 = t66 * t70 * t74
      t81 = t7 * x1
      t85 = t54 * t81 * t64 * t11 * t15
      t87 = t36 * t7
      t88 = x2 * t10
      t89 = 0.1D1 / t13
      t91 = t87 * t88 * t89
      t94 = t39 * t89
      t95 = z ** 2
      t98 = 0.1728D4 * t50 * t10 * t94 * t95
      t99 = t2 * t4
      t100 = t99 * t10
      t102 = t100 * t94 * x1
      t104 = t99 * x2
      t105 = x1 * t10
      t107 = t104 * t105 * t89
      t109 = t6 * t64
      t110 = t7 * t11
      t112 = t109 * t110 * t15
      t114 = t64 * x2
      t116 = t81 * t56
      t118 = t63 * t114 * t116 * t69
      t119 = 0.576D3 * t118
      t120 = -0.864D3 * t9 * t32 + 0.144D3 * t45 + 0.864D3 * t9 * t47 + 
     #t52 + 0.14976D5 * t60 + 0.14832D5 * t72 + 0.14976D5 * t76 + 0.1483
     #2D5 * t79 + 0.3466D4 * t85 - 0.1632D4 * t91 - t98 - 0.2304D4 * t10
     #2 + 0.224D3 * t107 - 0.128D3 * t112 - t119
      t121 = t2 * z
      t122 = t121 * t35
      t123 = x1 * t11
      t124 = x3 ** 2
      t125 = t123 * t124
      t126 = t122 * t125
      t128 = t20 ** 2
      t129 = t123 * t128
      t130 = t122 * t129
      t134 = t11 * x3 * t20
      t137 = t121 * t4
      t140 = t99 * t11
      t142 = t140 * t94 * t20
      t145 = t140 * t94 * x3
      t147 = t81 * t15
      t148 = t30 ** 2
      t149 = t147 * t148
      t151 = 0.288D3 * t122 * t149
      t155 = t20 * t17 * t13 + x2 * x3 + t29
      t156 = t155 ** 2
      t157 = t147 * t156
      t159 = 0.288D3 * t122 * t157
      t160 = t99 * t7
      t161 = z * t89
      t164 = 0.1728D4 * t160 * t161 * t30
      t167 = 0.1728D4 * t160 * t161 * t155
      t168 = t7 * t15
      t175 = x2 * t81
      t176 = t54 * t175
      t177 = t14 ** 2
      t178 = 0.1D1 / t177
      t179 = t11 * t178
      t182 = t176 * t179 * t39 * t148
      t186 = 0.1728D4 * t50 * x1 * t95
      t187 = t6 * t110
      t188 = t39 * t15
      t190 = t187 * t188 * t43
      t192 = t56 * t69
      t193 = t39 * t155
      t196 = t55 * t192 * t193 * x3
      t198 = 0.144D3 * t126 + 0.144D3 * t130 + 0.576D3 * t36 * x1 * t134
     # + 0.1152D4 * t137 * t134 - 0.1440D4 * t142 - 0.1440D4 * t145 + t1
     #51 + t159 - t164 - t167 - 0.576D3 * t137 * t168 * t148 - 0.576D3 *
     # t137 * t168 * t156 - 0.144D3 * t182 - t186 - 0.1440D4 * t190 - 0.
     #14976D5 * t196
      t200 = t20 * t155
      t202 = t187 * t188 * t200
      t204 = t16 * t200
      t205 = t176 * t204
      t209 = t30 * t155
      t210 = t10 * t69 * t209
      t214 = t121 * t4 * x1
      t215 = t89 * t30
      t216 = t10 * t20
      t217 = t215 * t216
      t221 = t121 * t4 * t10
      t222 = x3 * x1
      t226 = t20 * x1
      t227 = t89 * t155
      t231 = t10 * x3
      t232 = t227 * t231
      t236 = t63 * t64 * t81
      t237 = t56 * t178
      t238 = t39 * t30
      t240 = t236 * t237 * t238
      t243 = t187 * t188 * t31
      t245 = t176 * t32
      t249 = t55 * t192 * t58 * t30
      t252 = t236 * t237 * t193
      t254 = t6 * t40
      t255 = t69 * x2
      t256 = t7 * t155
      t258 = t254 * t255 * t256
      t261 = t121 * t35 * t7
      t262 = t216 * t227
      t264 = 0.288D3 * t261 * t262
      t266 = t121 * t35 * t10
      t267 = t39 * t69
      t269 = t267 * t7 * t156
      t270 = t266 * t269
      t272 = -0.864D3 * t202 - 0.14832D5 * t205 + 0.1152D4 * t6 * t175 *
     # t210 + 0.1152D4 * t214 * t217 - 0.1152D4 * t221 * t222 * t215 - 0
     #.1152D4 * t221 * t226 * t227 + 0.1152D4 * t214 * t232 + 0.144D3 * 
     #t240 - 0.864D3 * t243 - 0.14832D5 * t245 - 0.14832D5 * t249 + 0.14
     #4D3 * t252 - 0.3523D4 * t258 + t264 + 0.144D3 * t270
      t274 = 0.288D3 * t261 * t232
      t275 = t11 * t69
      t277 = t176 * t275 * t238
      t281 = t55 * t192 * t74 * t155
      t283 = t81 * t10
      t286 = t6 * t283 * t267 * t209
      t288 = t7 ** 2
      t291 = t54 * x2 * t288 * t210
      t293 = x1 * t56
      t295 = x3 * t20
      t296 = t94 * t295
      t297 = t6 * t293 * t296
      t300 = t56 * t89 * t295
      t301 = t55 * t300
      t303 = x1 * x2
      t304 = t54 * t303
      t305 = t67 * t15
      t308 = t304 * t305 * t58 * t20
      t310 = t56 * t39
      t311 = t6 * t310
      t312 = t15 * x2
      t314 = t311 * t312 * t222
      t317 = t311 * t312 * t226
      t323 = t121 * t35 * t11
      t327 = 0.288D3 * t323 * t188 * t226 * t155
      t328 = t7 * t30
      t331 = t266 * t267 * t328 * t155
      t333 = t155 * x3
      t335 = t41 * t42 * t333
      t337 = t16 * t333
      t340 = t15 * t20
      t341 = x1 * t155
      t343 = t41 * t340 * t341
      t345 = -t274 + 0.14976D5 * t277 - 0.14832D5 * t281 - 0.864D3 * t28
     #6 - 0.576D3 * t291 - 0.864D3 * t297 - 0.288D3 * t301 - 0.576D3 * t
     #308 + 0.1244D4 * t314 + 0.1244D4 * t317 + 0.1152D4 * t6 * t303 * t
     #300 + t327 - 0.288D3 * t331 + 0.144D3 * t335 + 0.864D3 * t9 * t337
     # - 0.144D3 * t343
      t351 = 0.1728D4 * t104 * t105 * t89 * t95
      t355 = 0.1728D4 * t109 * t110 * t15 * z
      t357 = t122 * t123 * t295
      t359 = t54 * x2
      t360 = t7 * t56
      t361 = t89 * t124
      t363 = t359 * t360 * t361
      t365 = t6 * x1
      t366 = t310 * t361
      t367 = t365 * t366
      t369 = t89 * t128
      t371 = t359 * t360 * t369
      t373 = t310 * t369
      t374 = t365 * t373
      t377 = 0.288D3 * t122 * t366
      t379 = 0.288D3 * t122 * t373
      t380 = t6 * t7
      t381 = x2 * t11
      t384 = t380 * t381 * t89 * x3
      t386 = t6 * x2
      t392 = t380 * t381 * t89 * t20
      t400 = 0.1728D4 * t140 * t94 * z * x3
      t404 = 0.1728D4 * t140 * t94 * z * t20
      t405 = -t351 - t355 - 0.288D3 * t357 - 0.144D3 * t363 - 0.432D3 * 
     #t367 - 0.144D3 * t371 - 0.432D3 * t374 + t377 + t379 - 0.3523D4 * 
     #t384 - 0.576D3 * t386 * t293 * t361 - 0.3523D4 * t392 - 0.576D3 * 
     #t386 * t293 * t369 - t400 - t404
      t406 = t63 * t64
      t408 = t406 * t116 * t340
      t410 = t15 * x3
      t412 = t406 * t116 * t410
      t414 = t288 * t11
      t417 = t406 * t414 * t69 * t30
      t421 = t406 * t414 * t69 * t155
      t423 = t6 * t81
      t424 = t10 * t39
      t425 = t69 * t148
      t427 = t423 * t424 * t425
      t429 = t69 * t156
      t431 = t423 * t424 * t429
      t433 = t15 * t30
      t435 = t423 * t88 * t433
      t440 = t87 * t217
      t442 = t36 * t10
      t444 = t267 * t7 * t148
      t447 = t231 * t215
      t448 = t87 * t447
      t452 = t423 * t88 * t15 * t155
      t457 = t87 * t262
      t461 = t87 * t232
      t463 = 0.144D3 * t408 + 0.144D3 * t412 + 0.14832D5 * t417 + 0.1483
     #2D5 * t421 - 0.432D3 * t427 - 0.432D3 * t431 + 0.1244D4 * t435 - 0
     #.576D3 * t386 * t283 * t425 + 0.144D3 * t440 - 0.288D3 * t442 * t4
     #44 - 0.144D3 * t448 + 0.1244D4 * t452 - 0.576D3 * t386 * t283 * t4
     #29 - 0.144D3 * t457 - 0.288D3 * t442 * t269 + 0.144D3 * t461
      t469 = t254 * t255 * t328
      t471 = x1 * t30
      t473 = t41 * t410 * t471
      t477 = 0.1440D4 * t50 * t424 * t89
      t482 = t4 * t11
      t489 = t7 * t89
      t491 = t99 * t489 * t30
      t494 = t99 * t489 * t155
      t498 = t176 * t179 * t238 * t155
      t503 = 0.288D3 * t323 * t188 * t222 * t30
      t507 = 0.288D3 * t323 * t188 * t471 * t20
      t509 = 0.288D3 * t261 * t217
      t510 = t266 * t444
      t513 = 0.288D3 * t261 * t447
      t514 = 0.1152D4 * t137 * t168 * t209 - 0.3523D4 * t469 - 0.144D3 *
     # t473 + t477 - 0.288D3 * t36 * t125 - 0.288D3 * t36 * t129 - 0.576
     #D3 * t121 * t482 * t128 - 0.576D3 * t121 * t482 * t124 - 0.1440D4 
     #* t491 - 0.1440D4 * t494 - 0.288D3 * t498 + t503 - t507 - t509 + 0
     #.144D3 * t510 + t513
      t518 = 0.288D3 * t323 * t188 * t341 * x3
      t521 = t6 * t11 * t188 * t8
      t525 = t54 * t56 * t267 * t65
      t527 = t36 * t11
      t529 = t527 * t188 * t303
      t531 = x1 * z
      t534 = 0.3456D4 * t100 * t94 * t531
      t537 = 0.1728D4 * t87 * t88 * t161
      t547 = 0.576D3 * t2 * t95 * z
      t549 = t187 * t188 * t333
      t551 = t176 * t337
      t555 = 0.1728D4 * t41 * t312 * t531
      t556 = t176 * t47
      t560 = t55 * t192 * t238 * t20
      t563 = t176 * t275 * t193
      t567 = t176 * t179 * t39 * t156
      t569 = -t518 + 0.7896D4 * t521 + 0.3466D4 * t525 - 0.1632D4 * t529
     # - t534 - t537 - 0.864D3 * t9 * t204 + 0.576D3 * t36 * t424 * t69 
     #* t7 * t209 - t547 - 0.1440D4 * t549 - 0.14976D5 * t551 - t555 - 0
     #.14976D5 * t556 - 0.14976D5 * t560 + 0.14976D5 * t563 - 0.144D3 * 
     #t567
      t578 = t2 * t5 * t35 * t114
      t593 = t36 * t56
      t596 = t288 * t10
      t614 = -0.72D2 * t45 + 0.3096D4 * t578 * t81 * t67 * t178 * t39 + 
     #0.6192D4 * t406 * t116 * t267 - 0.2808D4 * t527 * t94 * t222 - 0.2
     #808D4 * t527 * t94 * t226 - 0.576D3 * t593 * t296 + 0.432D3 * t359
     # * t596 * t425 + 0.432D3 * t359 * t596 * t429 - 0.576D3 * t122 * t
     #147 * t209 - 0.2808D4 * t87 * t424 * t433 - 0.2808D4 * t442 * t188
     # * t256 - t52 - 0.14616D5 * t60 - 0.15552D5 * t72
      t627 = -0.14616D5 * t76 - 0.15552D5 * t79 + 0.4940D4 * t85 + 0.481
     #6D4 * t91 + t98 + 0.4320D4 * t102 - 0.1376D4 * t107 + 0.1024D4 * t
     #112 + 0.352D3 * t118 - 0.288D3 * t126 - 0.288D3 * t130 + 0.1728D4 
     #* t142 + 0.1728D4 * t145 - t151 - t159
      t657 = t164 + t167 + 0.3096D4 * t578 * t288 * t56 * t69 - 0.288D3 
     #* t593 * t94 * t124 - 0.288D3 * t593 * t94 * t128 - 0.576D3 * t36 
     #* t81 * t433 * t155 - 0.936D3 * t182 + t186 + 0.1728D4 * t190 + 0.
     #432D3 * t304 * t305 * t39 * t128 + 0.432D3 * t304 * t305 * t39 * t
     #124 + 0.15552D5 * t196 + 0.2016D4 * t202 + 0.15696D5 * t205 - 0.21
     #60D4 * t240
      t671 = 0.2016D4 * t243 + 0.15696D5 * t245 + 0.15696D5 * t249 - 0.2
     #160D4 * t252 - 0.5266D4 * t258 + t264 - 0.288D3 * t270 - t274 - 0.
     #14616D5 * t277 + 0.15696D5 * t281 + 0.2016D4 * t286 + 0.864D3 * t2
     #91 + 0.2016D4 * t297 - 0.1872D4 * t301 + 0.864D3 * t308
      t688 = -0.3776D4 * t314 - 0.3776D4 * t317 - 0.576D3 * t121 * t35 *
     # t56 * t296 + t327 + 0.576D3 * t331 - 0.72D2 * t335 + 0.72D2 * t34
     #3 + t351 + t355 + 0.576D3 * t357 - 0.936D3 * t363 + 0.1008D4 * t36
     #7 - 0.936D3 * t371 + 0.1008D4 * t374
      t700 = -t377 - t379 - 0.5266D4 * t384 - 0.5266D4 * t392 + t400 + t
     #404 - 0.2160D4 * t408 - 0.2160D4 * t412 - 0.15552D5 * t417 - 0.155
     #52D5 * t421 + 0.1008D4 * t427 + 0.1008D4 * t431 - 0.3776D4 * t435 
     #- 0.72D2 * t440 + 0.72D2 * t448
      t715 = -0.3776D4 * t452 + 0.72D2 * t457 - 0.72D2 * t461 - 0.5266D4
     # * t469 + 0.72D2 * t473 - t477 + 0.1728D4 * t491 + 0.1728D4 * t494
     # - 0.288D3 * t36 * t149 - 0.288D3 * t36 * t157 - 0.1872D4 * t498 +
     # t503 - t507 - t509 - 0.288D3 * t510
      t725 = t513 - t518 + 0.9344D4 * t521 + 0.4940D4 * t525 + 0.4816D4 
     #* t529 + t534 + t537 + t547 + 0.1728D4 * t549 + 0.15552D5 * t551 +
     # t555 + 0.15552D5 * t556 + 0.15552D5 * t560 - 0.14616D5 * t563 - 0
     #.936D3 * t567
      t746 = 0.576D3 * t107 - 0.912D3 * t91 - 0.1728D4 * t521 - 0.424D3 
     #* t85 + t119 + 0.640D3 * t317 + 0.640D3 * t314 - 0.1152D4 * t112 +
     # 0.640D3 * t435 + 0.640D3 * t452 + 0.780D3 * t392 + 0.780D3 * t384
     # + 0.780D3 * t469 + 0.780D3 * t258 - 0.424D3 * t525 - 0.912D3 * t5
     #29
      rrgg2qqbarh11J3 = -(0.3D1 * wd * (t120 + t198 + t272 + t345 + t405
     # + t463 + t514 + t569) + 0.2D1 * wd * (t614 + t627 + t657 + t671 +
     # t688 + t700 + t715 + t725) + wd * t746) * nf / t1 / z / 0.3141592
     #653589793D1 / 0.96D2

      end function
  
   
 

      doubleprecision function rrgg2qqbarh11J4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t5 = t4 ** 2
      t7 = t2 * t5 * t3
      t8 = x1 ** 2
      t9 = t8 * x1
      t10 = x2 * t9
      t11 = t7 * t10
      t12 = 0.1D1 - x1
      t13 = t12 ** 2
      t15 = z + x1 * t3
      t16 = t15 ** 2
      t18 = 0.1D1 / t16 / t15
      t19 = t13 * t18
      t20 = 0.1D1 - x2
      t23 = z + x1 * t20 * t3
      t24 = x3 * t20
      t26 = 0.1D1 - x3
      t29 = cos(x4 * 0.3141592653589793D1)
      t33 = Sqrt(t24 * t15 * x2 * t26)
      t35 = 0.2D1 * t29 * t33
      t36 = t24 * t15 + x2 * t26 - t35
      t37 = t23 * t36
      t39 = t11 * t19 * t37
      t41 = t16 ** 2
      t42 = 0.1D1 / t41
      t43 = t13 * t42
      t44 = t36 ** 2
      t47 = t11 * t43 * t23 * t44
      t50 = t2 * t5 * t4
      t51 = x2 ** 2
      t53 = t50 * t51 * t9
      t54 = t13 * t12
      t55 = t54 * t42
      t57 = t53 * t55 * t37
      t59 = t2 * t5
      t60 = t8 * t13
      t61 = t59 * t60
      t62 = 0.1D1 / t16
      t63 = t23 * t62
      t64 = x3 * t36
      t66 = t61 * t63 * t64
      t68 = t13 * t62
      t69 = t68 * t64
      t70 = t11 * t69
      t72 = x2 * t8
      t73 = t7 * t72
      t74 = t54 * t18
      t75 = t23 * x3
      t78 = t73 * t74 * t75 * t36
      t80 = t36 * t26
      t82 = t61 * t63 * t80
      t84 = z ** 2
      t87 = 0.576D3 * t2 * t84 * z
      t88 = t2 * z
      t89 = t4 * t3
      t91 = t88 * t89 * t13
      t92 = t26 * x1
      t96 = t26 * t20 * t15 + x2 * x3 + t35
      t100 = 0.288D3 * t91 * t63 * t92 * t96
      t102 = t88 * t89 * t12
      t103 = t18 * t23
      t104 = t8 * t36
      t107 = t102 * t103 * t104 * t96
      t110 = t88 * t89 * t8
      t111 = t12 * t26
      t112 = 0.1D1 / t15
      t113 = t112 * t96
      t114 = t111 * t113
      t116 = 0.288D3 * t110 * t114
      t117 = t96 ** 2
      t119 = t103 * t8 * t117
      t120 = t102 * t119
      t122 = t12 * x3
      t123 = t113 * t122
      t125 = 0.288D3 * t110 * t123
      t126 = t13 * t23
      t127 = t59 * t126
      t128 = x2 * t18
      t130 = t127 * t128 * t104
      t132 = t2 * t3
      t134 = 0.1440D4 * t132 * x1
      t135 = 0.14976D5 * t39 - 0.144D3 * t47 + 0.144D3 * t57 - 0.864D3 *
     # t66 - 0.14832D5 * t70 - 0.14832D5 * t78 - 0.1440D4 * t82 - t87 + 
     #t100 - 0.288D3 * t107 + t116 + 0.144D3 * t120 - t125 - 0.3523D4 * 
     #t130 + t134
      t136 = t2 * t89
      t137 = t136 * t126
      t138 = t62 * x2
      t139 = x1 * z
      t142 = 0.1728D4 * t137 * t138 * t139
      t143 = t54 * t62
      t145 = t73 * t143 * t75
      t147 = t51 * t8
      t148 = t50 * t147
      t149 = t13 ** 2
      t150 = t149 * t18
      t152 = t148 * t150 * t75
      t154 = t23 * t26
      t156 = t73 * t143 * t154
      t159 = t148 * t150 * t154
      t161 = t54 * x1
      t163 = t23 * t112
      t164 = x3 * t26
      t165 = t163 * t164
      t166 = t59 * t161 * t165
      t169 = t54 * t112 * t164
      t170 = t73 * t169
      t172 = x1 * x2
      t173 = t7 * t172
      t174 = t149 * t62
      t177 = t173 * t174 * t75 * t26
      t179 = t54 * t23
      t180 = t59 * t179
      t181 = x3 * x1
      t183 = t180 * t138 * t181
      t186 = t180 * t138 * t92
      t191 = t59 * t72
      t192 = t96 * x3
      t193 = t68 * t192
      t196 = t62 * t26
      t197 = x1 * t96
      t199 = t137 * t196 * t197
      t201 = t26 * t96
      t202 = t68 * t201
      t205 = t12 * t23
      t208 = t36 * t96
      t214 = t12 * t18 * t208
      t217 = -t142 + 0.14976D5 * t145 + 0.14832D5 * t152 + 0.14976D5 * t
     #156 + 0.14832D5 * t159 - 0.864D3 * t166 - 0.288D3 * t170 - 0.576D3
     # * t177 + 0.1244D4 * t183 + 0.1244D4 * t186 + 0.1152D4 * t59 * t17
     #2 * t169 + 0.864D3 * t191 * t193 - 0.144D3 * t199 - 0.864D3 * t191
     # * t202 + 0.576D3 * t136 * t205 * t18 * t8 * t208 + 0.1152D4 * t59
     # * t10 * t214
      t220 = t88 * t4 * x1
      t221 = t112 * t36
      t222 = t221 * t111
      t226 = t88 * t4 * t12
      t235 = t62 * x3
      t236 = x1 * t36
      t238 = t137 * t235 * t236
      t242 = t62 * x1
      t244 = t137 * t242 * t80
      t246 = t68 * t80
      t249 = t8 * t96
      t251 = t127 * t128 * t249
      t254 = t137 * t242 * t192
      t258 = 0.1728D4 * t132 * x1 * t84
      t259 = t23 * t96
      t262 = t73 * t74 * t259 * x3
      t265 = t61 * t63 * t201
      t267 = t11 * t202
      t271 = t73 * t74 * t154 * t96
      t273 = 0.1152D4 * t220 * t222 - 0.1152D4 * t226 * t181 * t221 - 0.
     #1152D4 * t226 * t92 * t113 + 0.1152D4 * t220 * t123 - 0.144D3 * t2
     #38 - 0.864D3 * t191 * t69 + 0.144D3 * t244 + 0.864D3 * t191 * t246
     # - 0.3523D4 * t251 + 0.144D3 * t254 - t258 - 0.14976D5 * t262 - 0.
     #864D3 * t265 - 0.14832D5 * t267 - 0.14832D5 * t271
      t274 = t9 * t12
      t277 = t59 * t274 * t103 * t208
      t279 = t8 ** 2
      t282 = t7 * x2 * t279 * t214
      t286 = t11 * t43 * t37 * t96
      t291 = 0.288D3 * t91 * t63 * t181 * t36
      t295 = 0.288D3 * t91 * t63 * t236 * t26
      t297 = 0.288D3 * t110 * t222
      t299 = t103 * t8 * t44
      t300 = t102 * t299
      t302 = t122 * t221
      t304 = 0.288D3 * t110 * t302
      t308 = 0.288D3 * t91 * t63 * t197 * x3
      t309 = t11 * t246
      t313 = t73 * t74 * t37 * t26
      t316 = t11 * t19 * t259
      t320 = t11 * t43 * t23 * t117
      t323 = t53 * t55 * t259
      t326 = t61 * t63 * t192
      t328 = t11 * t193
      t330 = -0.864D3 * t277 - 0.576D3 * t282 - 0.288D3 * t286 + t291 - 
     #t295 - t297 + 0.144D3 * t300 + t304 - t308 - 0.14976D5 * t309 - 0.
     #14976D5 * t313 + 0.14976D5 * t316 - 0.144D3 * t320 + 0.144D3 * t32
     #3 - 0.1440D4 * t326 - 0.14976D5 * t328
      t335 = 0.1440D4 * t132 * t205 * t112
      t336 = t13 * x1
      t337 = x3 ** 2
      t338 = t336 * t337
      t341 = t26 ** 2
      t342 = t336 * t341
      t345 = t4 * t13
      t352 = t2 * t4
      t353 = t8 * t112
      t355 = t352 * t353 * t36
      t358 = t352 * t353 * t96
      t362 = t59 * t13 * t63 * t72
      t366 = t7 * t54 * t103 * t147
      t368 = t136 * t13
      t370 = t368 * t63 * t172
      t372 = t352 * t12
      t375 = 0.3456D4 * t372 * t163 * t139
      t376 = t136 * t8
      t377 = x2 * t12
      t378 = t112 * z
      t381 = 0.1728D4 * t376 * t377 * t378
      t382 = t352 * x2
      t383 = x1 * t12
      t387 = 0.1728D4 * t382 * t383 * t112 * t84
      t388 = t59 * t51
      t392 = 0.1728D4 * t388 * t60 * t62 * z
      t393 = t88 * t89
      t395 = t393 * t336 * t164
      t397 = t335 - 0.288D3 * t136 * t338 - 0.288D3 * t136 * t342 - 0.57
     #6D3 * t88 * t345 * t341 - 0.576D3 * t88 * t345 * t337 - 0.1440D4 *
     # t355 - 0.1440D4 * t358 + 0.7896D4 * t362 + 0.3466D4 * t366 - 0.16
     #32D4 * t370 - t375 - t381 - t387 - t392 - 0.288D3 * t395
      t398 = x2 * t7
      t399 = t54 * t8
      t400 = t112 * t337
      t402 = t398 * t399 * t400
      t404 = t59 * x1
      t405 = t179 * t400
      t406 = t404 * t405
      t408 = t112 * t341
      t410 = t398 * t399 * t408
      t412 = t179 * t408
      t413 = t404 * t412
      t416 = 0.288D3 * t393 * t405
      t418 = 0.288D3 * t393 * t412
      t419 = t59 * t8
      t420 = t13 * x2
      t423 = t419 * t420 * t112 * x3
      t425 = t59 * x2
      t431 = t419 * t420 * t112 * t26
      t436 = t352 * t13
      t440 = 0.1728D4 * t436 * t163 * z * x3
      t444 = 0.1728D4 * t436 * t163 * z * t26
      t445 = t50 * t51
      t446 = t9 * t54
      t448 = t445 * t446 * t196
      t451 = t445 * t446 * t235
      t453 = t279 * t13
      t456 = t445 * t453 * t18 * t36
      t460 = t445 * t453 * t18 * t96
      t462 = -0.144D3 * t402 - 0.432D3 * t406 - 0.144D3 * t410 - 0.432D3
     # * t413 + t416 + t418 - 0.3523D4 * t423 - 0.576D3 * t425 * t161 * 
     #t400 - 0.3523D4 * t431 - 0.576D3 * t425 * t161 * t408 - t440 - t44
     #4 + 0.144D3 * t448 + 0.144D3 * t451 + 0.14832D5 * t456 + 0.14832D5
     # * t460
      t464 = t59 * t9
      t465 = t44 * t18
      t467 = t464 * t205 * t465
      t469 = t18 * t117
      t471 = t464 * t205 * t469
      t473 = t62 * t36
      t475 = t464 * t377 * t473
      t480 = t376 * t222
      t482 = t136 * t12
      t485 = t376 * t302
      t489 = t464 * t377 * t62 * t96
      t494 = t376 * t114
      t498 = t376 * t123
      t500 = t88 * t4
      t501 = t8 * t62
      t508 = t7 * t9 * t51 * t13 * t62
      t511 = t376 * t377 * t112
      t516 = 0.1728D4 * t132 * t12 * t163 * t84
      t517 = -0.432D3 * t467 - 0.432D3 * t471 + 0.1244D4 * t475 - 0.576D
     #3 * t425 * t274 * t465 + 0.144D3 * t480 - 0.288D3 * t482 * t299 - 
     #0.144D3 * t485 + 0.1244D4 * t489 - 0.576D3 * t425 * t274 * t469 - 
     #0.144D3 * t494 - 0.288D3 * t482 * t119 + 0.144D3 * t498 + 0.1152D4
     # * t500 * t501 * t208 + 0.3466D4 * t508 - 0.1632D4 * t511 - t516
      t519 = t372 * t163 * x1
      t522 = t382 * t383 * t112
      t525 = t388 * t60 * t62
      t527 = t51 * x2
      t530 = t50 * t527 * t446 * t18
      t531 = 0.576D3 * t530
      t532 = t393 * t338
      t534 = t393 * t342
      t538 = t13 * x3 * t26
      t544 = t436 * t163 * t26
      t547 = t436 * t163 * x3
      t549 = t9 * t62
      t550 = t549 * t44
      t552 = 0.288D3 * t393 * t550
      t553 = t549 * t117
      t555 = 0.288D3 * t393 * t553
      t556 = t352 * t8
      t559 = 0.1728D4 * t556 * t378 * t36
      t562 = 0.1728D4 * t556 * t378 * t96
      t569 = -0.2304D4 * t519 + 0.224D3 * t522 - 0.128D3 * t525 - t531 +
     # 0.144D3 * t532 + 0.144D3 * t534 + 0.576D3 * t136 * x1 * t538 + 0.
     #1152D4 * t500 * t538 - 0.1440D4 * t544 - 0.1440D4 * t547 + t552 + 
     #t555 - t559 - t562 - 0.576D3 * t500 * t501 * t44 - 0.576D3 * t500 
     #* t501 * t117
      t585 = -0.14616D5 * t39 - 0.936D3 * t47 - 0.2160D4 * t57 + 0.2016D
     #4 * t66 + 0.15696D5 * t70 + 0.15696D5 * t78 + 0.1728D4 * t82 + t87
     # + t100 + 0.576D3 * t107 + t116 - 0.288D3 * t120 - t125 - 0.5266D4
     # * t130
      t599 = -t134 + t142 - 0.14616D5 * t145 - 0.15552D5 * t152 - 0.1461
     #6D5 * t156 - 0.15552D5 * t159 + 0.2016D4 * t166 - 0.1872D4 * t170 
     #+ 0.864D3 * t177 - 0.3776D4 * t183 - 0.3776D4 * t186 + 0.72D2 * t1
     #99 + 0.72D2 * t238 - 0.72D2 * t244 - 0.5266D4 * t251
      t610 = -0.72D2 * t254 + t258 + 0.15552D5 * t262 + 0.2016D4 * t265 
     #+ 0.15696D5 * t267 + 0.15696D5 * t271 + 0.2016D4 * t277 + 0.864D3 
     #* t282 - 0.1872D4 * t286 + t291 - t295 - t297 - 0.288D3 * t300 + t
     #304 - t308
      t636 = 0.432D3 * t173 * t174 * t23 * t337 + 0.432D3 * t173 * t174 
     #* t23 * t341 - 0.576D3 * t88 * t89 * t54 * t165 + 0.15552D5 * t309
     # + 0.15552D5 * t313 - 0.14616D5 * t316 - 0.936D3 * t320 - 0.2160D4
     # * t323 + 0.1728D4 * t326 + 0.15552D5 * t328 - t335 + 0.1728D4 * t
     #355 + 0.1728D4 * t358 - 0.288D3 * t136 * t550 - 0.288D3 * t136 * t
     #553
      t647 = 0.9344D4 * t362 + 0.4940D4 * t366 + 0.4816D4 * t370 + t375 
     #+ t381 + t387 + t392 + 0.576D3 * t395 - 0.936D3 * t402 + 0.1008D4 
     #* t406 - 0.936D3 * t410 + 0.1008D4 * t413 - t416 - t418
      t661 = -0.5266D4 * t423 - 0.5266D4 * t431 + t440 + t444 - 0.2160D4
     # * t448 - 0.2160D4 * t451 - 0.15552D5 * t456 - 0.15552D5 * t460 + 
     #0.1008D4 * t467 + 0.1008D4 * t471 - 0.3776D4 * t475 - 0.72D2 * t48
     #0 + 0.72D2 * t485 - 0.3776D4 * t489 + 0.72D2 * t494
      t666 = t2 * t5 * t89 * t527
      t681 = t136 * t54
      t684 = t279 * t12
      t703 = -0.72D2 * t498 + 0.3096D4 * t666 * t9 * t149 * t42 * t23 + 
     #0.6192D4 * t445 * t446 * t103 - 0.2808D4 * t368 * t163 * t181 - 0.
     #2808D4 * t368 * t163 * t92 - 0.576D3 * t681 * t165 + 0.432D3 * t39
     #8 * t684 * t465 + 0.432D3 * t398 * t684 * t469 - 0.576D3 * t393 * 
     #t549 * t208 - 0.2808D4 * t376 * t205 * t473 - 0.2808D4 * t482 * t6
     #3 * t249 + 0.4940D4 * t508 + 0.4816D4 * t511 + t516 + 0.4320D4 * t
     #519
      t725 = -0.1376D4 * t522 + 0.1024D4 * t525 + 0.352D3 * t530 - 0.288
     #D3 * t532 - 0.288D3 * t534 + 0.1728D4 * t544 + 0.1728D4 * t547 - t
     #552 - t555 + t559 + t562 + 0.3096D4 * t666 * t279 * t54 * t18 - 0.
     #288D3 * t681 * t163 * t337 - 0.288D3 * t681 * t163 * t341 - 0.576D
     #3 * t136 * t9 * t473 * t96
      t746 = 0.576D3 * t522 - 0.912D3 * t511 - 0.1728D4 * t362 - 0.424D3
     # * t508 + t531 + 0.640D3 * t186 + 0.640D3 * t183 - 0.1152D4 * t525
     # + 0.640D3 * t475 + 0.640D3 * t489 + 0.780D3 * t431 + 0.780D3 * t4
     #23 + 0.780D3 * t130 + 0.780D3 * t251 - 0.424D3 * t366 - 0.912D3 * 
     #t370
      rrgg2qqbarh11J4 = -(0.4D1 * wd * (t135 + t217 + t273 + t330 + t397
     # + t462 + t517 + t569) + 0.3D1 * wd * (t585 + t599 + t610 + t636 +
     # t647 + t661 + t703 + t725) + 0.2D1 * wd * t746) * nf / t1 / z / 0
     #.3141592653589793D1 / 0.96D2

      end function
  
   
 

      doubleprecision function rrgg2qqbarh11J5
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t5 = t4 ** 2
      t7 = t2 * t5 * t4
      t8 = x2 ** 2
      t9 = t7 * t8
      t10 = x1 ** 2
      t11 = t10 * x1
      t12 = 0.1D1 - x1
      t13 = t12 ** 2
      t14 = t13 * t12
      t15 = t11 * t14
      t17 = z + x1 * t3
      t18 = t17 ** 2
      t19 = 0.1D1 / t18
      t20 = 0.1D1 - x3
      t21 = t19 * t20
      t23 = t9 * t15 * t21
      t25 = t2 * t4
      t26 = t25 * t13
      t27 = 0.1D1 - x2
      t30 = z + x1 * t27 * t3
      t31 = 0.1D1 / t17
      t32 = t30 * t31
      t36 = 0.1728D4 * t26 * t32 * z * t20
      t40 = 0.1728D4 * t26 * t32 * z * x3
      t41 = t2 * t5
      t42 = t41 * x2
      t43 = x1 * t14
      t44 = t20 ** 2
      t45 = t31 * t44
      t49 = t41 * t10
      t50 = x2 * t13
      t53 = t49 * t50 * t31 * t20
      t55 = x3 ** 2
      t56 = t31 * t55
      t62 = t49 * t50 * t31 * x3
      t64 = t2 * z
      t65 = t4 * t3
      t66 = t64 * t65
      t67 = t14 * t30
      t68 = t67 * t45
      t70 = 0.288D3 * t66 * t68
      t71 = t67 * t56
      t73 = 0.288D3 * t66 * t71
      t74 = t41 * x1
      t75 = t74 * t68
      t78 = t2 * t5 * t3
      t79 = t78 * x2
      t80 = t10 * t14
      t82 = t79 * t80 * t45
      t84 = t74 * t71
      t87 = t79 * t80 * t56
      t89 = x1 * t13
      t90 = x3 * t20
      t92 = t66 * t89 * t90
      t94 = t41 * t8
      t95 = t10 * t13
      t99 = 0.1728D4 * t94 * t95 * t19 * z
      t100 = 0.144D3 * t23 - t36 - t40 - 0.576D3 * t42 * t43 * t45 - 0.3
     #523D4 * t53 - 0.576D3 * t42 * t43 * t56 - 0.3523D4 * t62 + t70 + t
     #73 - 0.432D3 * t75 - 0.144D3 * t82 - 0.432D3 * t84 - 0.144D3 * t87
     # - 0.288D3 * t92 - t99
      t101 = t25 * x2
      t102 = x1 * t12
      t103 = z ** 2
      t107 = 0.1728D4 * t101 * t102 * t31 * t103
      t108 = t2 * t65
      t109 = t108 * t10
      t110 = x2 * t12
      t111 = t31 * z
      t114 = 0.1728D4 * t109 * t110 * t111
      t115 = t25 * t12
      t116 = x1 * z
      t119 = 0.3456D4 * t115 * t32 * t116
      t120 = t108 * t13
      t121 = t30 * t19
      t122 = x1 * x2
      t124 = t120 * t121 * t122
      t128 = 0.1D1 / t18 / t17
      t129 = t30 * t128
      t130 = t8 * t10
      t132 = t78 * t14 * t129 * t130
      t135 = t10 * x2
      t137 = t41 * t13 * t121 * t135
      t139 = t108 * t12
      t140 = x3 * t27
      t144 = cos(x4 * 0.3141592653589793D1)
      t148 = Sqrt(t140 * t17 * x2 * t20)
      t150 = 0.2D1 * t144 * t148
      t151 = t140 * t17 + x2 * t20 - t150
      t152 = t151 ** 2
      t154 = t129 * t10 * t152
      t157 = t31 * t151
      t158 = t12 * t20
      t159 = t157 * t158
      t160 = t109 * t159
      t162 = t11 * t12
      t163 = t128 * t152
      t167 = t41 * t11
      t168 = t19 * t151
      t170 = t167 * t110 * t168
      t172 = t12 * t30
      t176 = t20 * t27 * t17 + x2 * x3 + t150
      t177 = t176 ** 2
      t178 = t128 * t177
      t180 = t167 * t172 * t178
      t183 = t167 * t172 * t163
      t185 = t10 ** 2
      t186 = t185 * t13
      t189 = t9 * t186 * t128 * t176
      t193 = t9 * t186 * t128 * t151
      t195 = t19 * x3
      t197 = t9 * t15 * t195
      t199 = t64 * t4
      t200 = t10 * t19
      t201 = t151 * t176
      t205 = -t107 - t114 - t119 - 0.1632D4 * t124 + 0.3466D4 * t132 + 0
     #.7896D4 * t137 - 0.288D3 * t139 * t154 + 0.144D3 * t160 - 0.576D3 
     #* t42 * t162 * t163 + 0.1244D4 * t170 - 0.432D3 * t180 - 0.432D3 *
     # t183 + 0.14832D5 * t189 + 0.14832D5 * t193 + 0.144D3 * t197 + 0.1
     #152D4 * t199 * t200 * t201
      t207 = t31 * t176
      t208 = t12 * x3
      t209 = t207 * t208
      t210 = t109 * t209
      t213 = t129 * t10 * t177
      t216 = t158 * t207
      t217 = t109 * t216
      t224 = t167 * t110 * t19 * t176
      t226 = t208 * t157
      t227 = t109 * t226
      t230 = t64 * t65 * t12
      t231 = t10 * t151
      t234 = t230 * t129 * t231 * t176
      t237 = t64 * t65 * t10
      t239 = 0.288D3 * t237 * t216
      t240 = t230 * t213
      t243 = 0.288D3 * t237 * t209
      t244 = t41 * t95
      t245 = t151 * t20
      t247 = t244 * t121 * t245
      t249 = x2 * t11
      t250 = t78 * t249
      t251 = t13 * t19
      t252 = t251 * t245
      t253 = t250 * t252
      t256 = t64 * t4 * t12
      t257 = x3 * x1
      t261 = t20 * x1
      t270 = 0.144D3 * t210 - 0.288D3 * t139 * t213 - 0.144D3 * t217 - 0
     #.576D3 * t42 * t162 * t178 + 0.1244D4 * t224 - 0.144D3 * t227 - 0.
     #288D3 * t234 + t239 + 0.144D3 * t240 - t243 - 0.1440D4 * t247 - 0.
     #14976D5 * t253 - 0.1152D4 * t256 * t257 * t157 - 0.1152D4 * t256 *
     # t261 * t207 + 0.576D3 * t108 * t172 * t128 * t10 * t201
      t273 = t12 * t128 * t201
      t277 = t64 * t4 * x1
      t280 = t18 ** 2
      t281 = 0.1D1 / t280
      t282 = t13 * t281
      t285 = t250 * t282 * t30 * t152
      t287 = t2 * t3
      t290 = 0.1728D4 * t287 * x1 * t103
      t291 = t20 * t176
      t293 = t244 * t121 * t291
      t295 = t13 * t128
      t296 = t30 * t176
      t298 = t250 * t295 * t296
      t302 = t250 * t282 * t30 * t177
      t305 = t7 * t8 * t11
      t306 = t14 * t281
      t307 = t30 * t151
      t309 = t305 * t306 * t307
      t311 = t78 * t135
      t312 = t14 * t19
      t313 = t30 * t20
      t315 = t311 * t312 * t313
      t317 = t11 * t19
      t318 = t317 * t177
      t320 = 0.288D3 * t66 * t318
      t321 = t25 * t10
      t324 = 0.1728D4 * t321 * t111 * t151
      t327 = 0.1728D4 * t321 * t111 * t176
      t338 = t41 * t162 * t129 * t201
      t340 = 0.1152D4 * t41 * t249 * t273 + 0.1152D4 * t277 * t159 - 0.1
     #44D3 * t285 - t290 - 0.864D3 * t293 + 0.14976D5 * t298 - 0.144D3 *
     # t302 + 0.144D3 * t309 + 0.14976D5 * t315 + t320 - t324 - t327 - 0
     #.576D3 * t199 * t200 * t152 - 0.576D3 * t199 * t200 * t177 + 0.115
     #2D4 * t277 * t209 - 0.864D3 * t338
      t345 = t78 * x2 * t185 * t273
      t349 = t250 * t282 * t307 * t176
      t351 = t251 * t291
      t352 = t250 * t351
      t354 = t14 * t128
      t357 = t311 * t354 * t313 * t176
      t359 = t13 * t30
      t360 = t41 * t359
      t361 = t128 * x2
      t363 = t360 * t361 * t231
      t365 = t108 * t359
      t366 = x1 * t151
      t368 = t365 * t195 * t366
      t370 = t41 * t135
      t371 = x3 * t151
      t372 = t251 * t371
      t376 = t64 * t65 * t13
      t380 = 0.288D3 * t376 * t121 * t257 * t151
      t384 = 0.288D3 * t376 * t121 * t366 * t20
      t386 = 0.288D3 * t237 * t159
      t387 = t230 * t154
      t390 = 0.288D3 * t237 * t226
      t391 = x1 * t176
      t395 = 0.288D3 * t376 * t121 * t391 * x3
      t398 = t14 * t31 * t90
      t402 = t250 * t295 * t307
      t404 = -0.576D3 * t345 - 0.288D3 * t349 - 0.14832D5 * t352 - 0.148
     #32D5 * t357 - 0.3523D4 * t363 - 0.144D3 * t368 - 0.864D3 * t370 * 
     #t372 + t380 - t384 - t386 + 0.144D3 * t387 + t390 - t395 + 0.1152D
     #4 * t41 * t122 * t398 + 0.14976D5 * t402
      t406 = t305 * t306 * t296
      t408 = t176 * x3
      t410 = t244 * t121 * t408
      t412 = t251 * t408
      t413 = t250 * t412
      t416 = t244 * t121 * t371
      t418 = t250 * t372
      t420 = t30 * x3
      t423 = t311 * t354 * t420 * t151
      t428 = 0.288D3 * t376 * t121 * t261 * t176
      t429 = t19 * x1
      t431 = t365 * t429 * t245
      t436 = 0.1440D4 * t287 * x1
      t440 = t78 * t11 * t8 * t13 * t19
      t445 = 0.1728D4 * t287 * t12 * t32 * t103
      t447 = t109 * t110 * t31
      t451 = 0.1440D4 * t287 * t172 * t31
      t452 = t89 * t44
      t455 = t89 * t55
      t458 = 0.144D3 * t406 - 0.1440D4 * t410 - 0.14976D5 * t413 - 0.864
     #D3 * t416 - 0.14832D5 * t418 - 0.14832D5 * t423 + t428 + 0.144D3 *
     # t431 + 0.864D3 * t370 * t252 + t436 + 0.3466D4 * t440 - t445 - 0.
     #1632D4 * t447 + t451 - 0.288D3 * t108 * t452 - 0.288D3 * t108 * t4
     #55
      t460 = t10 * t31
      t462 = t25 * t460 * t151
      t464 = t4 * t13
      t472 = t115 * t32 * x1
      t476 = t311 * t354 * t307 * t20
      t479 = t101 * t102 * t31
      t482 = t25 * t460 * t176
      t485 = t94 * t95 * t19
      t487 = t8 * x2
      t490 = t7 * t487 * t15 * t128
      t491 = 0.576D3 * t490
      t492 = t7 * t130
      t493 = t13 ** 2
      t494 = t493 * t128
      t496 = t492 * t494 * t313
      t500 = t13 * x3 * t20
      t503 = t66 * t452
      t505 = t66 * t455
      t510 = t26 * t32 * t20
      t513 = t32 * t90
      t514 = t41 * t43 * t513
      t516 = -0.1440D4 * t462 - 0.576D3 * t64 * t464 * t55 - 0.576D3 * t
     #64 * t464 * t44 - 0.2304D4 * t472 - 0.14976D5 * t476 + 0.224D3 * t
     #479 - 0.1440D4 * t482 - 0.128D3 * t485 - t491 + 0.14832D5 * t496 +
     # 0.576D3 * t108 * x1 * t500 + 0.144D3 * t503 + 0.144D3 * t505 + 0.
     #1152D4 * t199 * t500 - 0.1440D4 * t510 - 0.864D3 * t514
      t517 = t311 * t398
      t519 = t78 * t122
      t520 = t493 * t19
      t523 = t519 * t520 * t420 * t20
      t525 = t41 * t67
      t526 = t19 * x2
      t528 = t525 * t526 * t257
      t531 = t525 * t526 * t261
      t534 = t26 * t32 * x3
      t538 = 0.576D3 * t2 * t103 * z
      t541 = 0.1728D4 * t365 * t526 * t116
      t543 = t311 * t312 * t420
      t546 = t492 * t494 * t420
      t548 = t317 * t152
      t550 = 0.288D3 * t66 * t548
      t553 = t311 * t354 * t296 * x3
      t555 = t10 * t176
      t557 = t360 * t361 * t555
      t560 = t365 * t429 * t408
      t565 = t365 * t21 * t391
      t569 = -0.288D3 * t517 - 0.576D3 * t523 + 0.1244D4 * t528 + 0.1244
     #D4 * t531 - 0.1440D4 * t534 - t538 - t541 + 0.14976D5 * t543 + 0.1
     #4832D5 * t546 + t550 - 0.14976D5 * t553 - 0.3523D4 * t557 + 0.144D
     #3 * t560 + 0.864D3 * t370 * t412 - 0.144D3 * t565 - 0.864D3 * t370
     # * t351
      t583 = -0.2160D4 * t23 + t36 + t40 - 0.5266D4 * t53 - 0.5266D4 * t
     #62 - t70 - t73 + 0.1008D4 * t75 - 0.936D3 * t82 + 0.1008D4 * t84 -
     # 0.936D3 * t87 + 0.576D3 * t92 + t99 + t107
      t597 = t114 + t119 + 0.4816D4 * t124 + 0.4940D4 * t132 + 0.9344D4 
     #* t137 - 0.72D2 * t160 - 0.3776D4 * t170 + 0.1008D4 * t180 + 0.100
     #8D4 * t183 - 0.15552D5 * t189 - 0.15552D5 * t193 - 0.2160D4 * t197
     # - 0.72D2 * t210 + 0.72D2 * t217 - 0.3776D4 * t224
      t616 = 0.72D2 * t227 + 0.576D3 * t234 + t239 - 0.288D3 * t240 - t2
     #43 + 0.1728D4 * t247 + 0.15552D5 * t253 - 0.936D3 * t285 - 0.288D3
     # * t108 * t548 + t290 + 0.2016D4 * t293 - 0.14616D5 * t298 - 0.936
     #D3 * t302 + 0.6192D4 * t9 * t15 * t129 - 0.2808D4 * t120 * t32 * t
     #257
      t617 = t185 * t12
      t634 = t2 * t5 * t65 * t487
      t639 = t108 * t14
      t652 = 0.432D3 * t79 * t617 * t178 - 0.576D3 * t66 * t317 * t201 -
     # 0.2808D4 * t139 * t121 * t555 - 0.2808D4 * t109 * t172 * t168 - 0
     #.2160D4 * t309 - 0.14616D5 * t315 - t320 + t324 + t327 + 0.3096D4 
     #* t634 * t185 * t14 * t128 - 0.288D3 * t639 * t32 * t55 - 0.288D3 
     #* t639 * t32 * t44 - 0.576D3 * t108 * t11 * t168 * t176 + 0.2016D4
     # * t338 + 0.864D3 * t345
      t664 = -0.1872D4 * t349 + 0.15696D5 * t352 + 0.15696D5 * t357 - 0.
     #5266D4 * t363 + 0.72D2 * t368 + t380 - t384 - t386 - 0.288D3 * t38
     #7 + t390 - t395 - 0.14616D5 * t402 - 0.2160D4 * t406 + 0.1728D4 * 
     #t410
      t676 = 0.15552D5 * t413 + 0.2016D4 * t416 + 0.15696D5 * t418 + 0.1
     #5696D5 * t423 + t428 - 0.72D2 * t431 - t436 + 0.4940D4 * t440 + t4
     #45 + 0.4816D4 * t447 - t451 + 0.1728D4 * t462 + 0.4320D4 * t472 + 
     #0.15552D5 * t476 - 0.1376D4 * t479
      t700 = 0.1728D4 * t482 + 0.1024D4 * t485 + 0.352D3 * t490 + 0.3096
     #D4 * t634 * t11 * t493 * t281 * t30 - 0.15552D5 * t496 - 0.288D3 *
     # t503 - 0.288D3 * t505 - 0.576D3 * t64 * t65 * t14 * t513 + 0.1728
     #D4 * t510 + 0.2016D4 * t514 - 0.1872D4 * t517 + 0.864D3 * t523 - 0
     #.3776D4 * t528 - 0.3776D4 * t531 + 0.1728D4 * t534
      t725 = t538 + t541 - 0.14616D5 * t543 - 0.15552D5 * t546 - 0.288D3
     # * t108 * t318 - t550 + 0.15552D5 * t553 + 0.432D3 * t519 * t520 *
     # t30 * t44 - 0.5266D4 * t557 - 0.72D2 * t560 + 0.72D2 * t565 + 0.4
     #32D3 * t519 * t520 * t30 * t55 + 0.432D3 * t79 * t617 * t163 - 0.5
     #76D3 * t639 * t513 - 0.2808D4 * t120 * t32 * t261
      t746 = 0.576D3 * t479 - 0.912D3 * t447 - 0.1728D4 * t137 - 0.424D3
     # * t440 + t491 + 0.640D3 * t531 + 0.640D3 * t528 - 0.1152D4 * t485
     # + 0.640D3 * t170 + 0.640D3 * t224 + 0.780D3 * t53 + 0.780D3 * t62
     # + 0.780D3 * t363 + 0.780D3 * t557 - 0.424D3 * t132 - 0.912D3 * t1
     #24
      rrgg2qqbarh11J5 = -(0.5D1 * wd * (t100 + t205 + t270 + t340 + t404
     # + t458 + t516 + t569) + 0.4D1 * wd * (t583 + t597 + t616 + t652 +
     # t664 + t676 + t700 + t725) + 0.3D1 * wd * t746) * nf / t1 / z / 0
     #.3141592653589793D1 / 0.96D2

      end function
  
   
 

      doubleprecision function rrgg2qqbarh11J6
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = z ** 2
      t7 = t2 * z
      t8 = 0.1D1 - z
      t9 = t8 ** 2
      t10 = t9 * t8
      t11 = x1 ** 2
      t13 = t7 * t10 * t11
      t15 = z + x1 * t8
      t16 = 0.1D1 / t15
      t17 = 0.1D1 - x3
      t18 = 0.1D1 - x2
      t23 = cos(x4 * 0.3141592653589793D1)
      t24 = x3 * t18
      t28 = Sqrt(t24 * t15 * x2 * t17)
      t30 = 0.2D1 * t23 * t28
      t31 = t17 * t18 * t15 + x2 * x3 + t30
      t32 = t16 * t31
      t33 = 0.1D1 - x1
      t34 = t33 * x3
      t35 = t32 * t34
      t38 = t9 ** 2
      t39 = t2 * t38
      t40 = t33 ** 2
      t43 = z + x1 * t18 * t8
      t44 = t40 * t43
      t45 = t39 * t44
      t46 = t15 ** 2
      t48 = 0.1D1 / t46 / t15
      t49 = t48 * x2
      t52 = t24 * t15 + x2 * t17 - t30
      t53 = t11 * t52
      t55 = t45 * t49 * t53
      t57 = t2 * t10
      t58 = t57 * t44
      t59 = 0.1D1 / t46
      t60 = t59 * x3
      t61 = x1 * t52
      t65 = t59 * x1
      t66 = t52 * t17
      t71 = t2 * t38 * t8
      t72 = t11 * x1
      t74 = x2 ** 2
      t77 = t71 * t72 * t74 * t40 * t59
      t79 = t57 * t11
      t80 = x2 * t33
      t82 = t79 * t80 * t16
      t84 = t2 * t8
      t86 = t43 * t16
      t90 = t2 * t9
      t91 = t90 * t33
      t95 = t90 * x2
      t96 = x1 * t33
      t98 = t95 * t96 * t16
      t100 = t39 * t74
      t101 = t11 * t40
      t103 = t100 * t101 * t59
      t106 = t2 * t38 * t9
      t107 = t74 * x2
      t109 = t40 * t33
      t110 = t72 * t109
      t112 = t106 * t107 * t110 * t48
      t114 = t7 * t10
      t115 = x1 * t40
      t116 = x3 ** 2
      t120 = t17 ** 2
      t124 = 0.576D3 * t2 * t3 * z - 0.288D3 * t13 * t35 - 0.5266D4 * t5
     #5 + 0.72D2 * t58 * t60 * t61 - 0.72D2 * t58 * t65 * t66 + 0.4940D4
     # * t77 + 0.4816D4 * t82 + 0.1728D4 * t84 * t33 * t86 * t3 + 0.4320
     #D4 * t91 * t86 * x1 - 0.1376D4 * t98 + 0.1024D4 * t103 + 0.352D3 *
     # t112 - 0.288D3 * t114 * t115 * t116 - 0.288D3 * t114 * t115 * t12
     #0
      t125 = t90 * t40
      t132 = t72 * t59
      t133 = t52 ** 2
      t134 = t132 * t133
      t137 = t31 ** 2
      t138 = t132 * t137
      t141 = t90 * t11
      t142 = z * t16
      t151 = t2 * t38 * t10 * t107
      t152 = t11 ** 2
      t157 = t57 * t109
      t164 = x1 * x2
      t165 = t71 * t164
      t166 = t40 ** 2
      t167 = t166 * t59
      t177 = t71 * x2 * t72
      t178 = t40 * t48
      t179 = t43 * t31
      t183 = t46 ** 2
      t184 = 0.1D1 / t183
      t185 = t40 * t184
      t193 = t52 * t31
      t197 = t43 * t52
      t202 = 0.1728D4 * t125 * t86 * t17 + 0.1728D4 * t125 * t86 * x3 - 
     #0.288D3 * t114 * t134 - 0.288D3 * t114 * t138 + 0.1728D4 * t141 * 
     #t142 * t52 + 0.1728D4 * t141 * t142 * t31 + 0.3096D4 * t151 * t152
     # * t109 * t48 - 0.288D3 * t157 * t86 * t116 - 0.288D3 * t157 * t86
     # * t120 + 0.432D3 * t165 * t167 * t43 * t116 + 0.432D3 * t165 * t1
     #67 * t43 * t120 - 0.14616D5 * t177 * t178 * t179 - 0.936D3 * t177 
     #* t185 * t43 * t137 + 0.864D3 * t71 * x2 * t152 * t33 * t48 * t193
     # - 0.1872D4 * t177 * t185 * t197 * t31
      t205 = t7 * t10 * t40
      t206 = t43 * t59
      t207 = x3 * x1
      t216 = t16 * t52
      t217 = t33 * t17
      t218 = t216 * t217
      t221 = t109 * t43
      t222 = t39 * t221
      t223 = t59 * x2
      t225 = t222 * t223 * t207
      t227 = x1 * t17
      t229 = t222 * t223 * t227
      t239 = t106 * t74 * t72
      t240 = t109 * t184
      t244 = t39 * t101
      t245 = x3 * t52
      t249 = t17 * t31
      t253 = t40 * t59
      t257 = x2 * t11
      t258 = t71 * t257
      t259 = t109 * t59
      t260 = t43 * x3
      t264 = t74 * t11
      t265 = t106 * t264
      t266 = t166 * t48
      t270 = t43 * t17
      t277 = 0.288D3 * t205 * t206 * t207 * t52 - 0.288D3 * t205 * t206 
     #* t61 * t17 - 0.288D3 * t13 * t218 - 0.3776D4 * t225 - 0.3776D4 * 
     #t229 - 0.14616D5 * t177 * t178 * t197 - 0.936D3 * t177 * t185 * t4
     #3 * t133 - 0.2160D4 * t239 * t240 * t197 + 0.2016D4 * t244 * t206 
     #* t245 + 0.2016D4 * t244 * t206 * t249 + 0.15696D5 * t177 * t253 *
     # t249 - 0.14616D5 * t258 * t259 * t260 - 0.15552D5 * t265 * t266 *
     # t260 - 0.14616D5 * t258 * t259 * t270 - 0.15552D5 * t265 * t266 *
     # t270
      t280 = x3 * t17
      t281 = t86 * t280
      t294 = t33 * t43
      t295 = t59 * t52
      t300 = t11 * t31
      t304 = t109 * t48
      t309 = t217 * t32
      t313 = t7 * t10 * t33
      t314 = t43 * t48
      t319 = x1 * z
      t340 = 0.2016D4 * t39 * x1 * t109 * t281 + 0.1728D4 * t84 * x1 * t
     #3 - 0.576D3 * t7 * t10 * t109 * t281 - 0.576D3 * t114 * t132 * t19
     #3 - 0.2808D4 * t79 * t294 * t295 - 0.2808D4 * t57 * t33 * t206 * t
     #300 + 0.15696D5 * t258 * t304 * t270 * t31 + 0.288D3 * t13 * t309 
     #- 0.288D3 * t313 * t314 * t11 * t137 + 0.1728D4 * t58 * t223 * t31
     #9 + 0.15696D5 * t177 * t253 * t245 + 0.15696D5 * t258 * t304 * t26
     #0 * t52 + 0.1728D4 * t244 * t206 * t66 + 0.15552D5 * t177 * t253 *
     # t66 + 0.15552D5 * t258 * t304 * t197 * t17
      t354 = t45 * t49 * t300
      t356 = t31 * x3
      t360 = t59 * t17
      t361 = x1 * t31
      t369 = t34 * t216
      t394 = -0.1872D4 * t258 * t109 * t16 * t280 + 0.864D3 * t165 * t16
     #7 * t260 * t17 - 0.1440D4 * t84 * x1 - 0.5266D4 * t354 - 0.72D2 * 
     #t58 * t65 * t356 + 0.72D2 * t58 * t360 * t361 - 0.288D3 * t313 * t
     #314 * t11 * t133 + 0.288D3 * t13 * t369 - 0.288D3 * t205 * t206 * 
     #t361 * x3 + 0.288D3 * t205 * t206 * t227 * t31 + 0.576D3 * t313 * 
     #t314 * t53 * t31 + 0.15552D5 * t177 * t253 * t356 + 0.15552D5 * t2
     #58 * t304 * t179 * x3 - 0.2160D4 * t239 * t240 * t179
      t400 = t39 * t40 * t206 * t257
      t404 = t71 * t109 * t314 * t264
      t406 = t57 * t40
      t408 = t406 * t206 * t164
      t427 = t71 * x2
      t428 = t11 * t109
      t429 = t16 * t116
      t433 = t39 * x1
      t434 = t221 * t429
      t437 = t16 * t120
      t441 = t221 * t437
      t448 = 0.1728D4 * t244 * t206 * t356 + 0.9344D4 * t400 + 0.4940D4 
     #* t404 + 0.4816D4 * t408 + 0.3456D4 * t91 * t86 * t319 + 0.1728D4 
     #* t79 * t80 * t142 + 0.1728D4 * t95 * t96 * t16 * t3 + 0.1728D4 * 
     #t100 * t101 * t59 * z + 0.576D3 * t114 * t115 * t280 - 0.936D3 * t
     #427 * t428 * t429 + 0.1008D4 * t433 * t434 - 0.936D3 * t427 * t428
     # * t437 + 0.1008D4 * t433 * t441 - 0.288D3 * t114 * t434 - 0.288D3
     # * t114 * t441
      t450 = t39 * t11
      t451 = x2 * t40
      t454 = t450 * t451 * t16 * x3
      t458 = t450 * t451 * t16 * t17
      t468 = t106 * t74
      t475 = t152 * t40
      t484 = t39 * t72
      t485 = t48 * t133
      t489 = t48 * t137
      t494 = t484 * t80 * t295
      t502 = t484 * t80 * t59 * t31
      t506 = -0.5266D4 * t454 - 0.5266D4 * t458 + 0.1728D4 * t125 * t86 
     #* z * x3 + 0.1728D4 * t125 * t86 * z * t17 - 0.2160D4 * t468 * t11
     #0 * t360 - 0.2160D4 * t468 * t110 * t60 - 0.15552D5 * t468 * t475 
     #* t48 * t52 - 0.15552D5 * t468 * t475 * t48 * t31 + 0.1008D4 * t48
     #4 * t294 * t485 + 0.1008D4 * t484 * t294 * t489 - 0.3776D4 * t494 
     #- 0.72D2 * t79 * t218 + 0.72D2 * t79 * t369 - 0.3776D4 * t502 + 0.
     #72D2 * t79 * t309
      t525 = t152 * t33
      t544 = t11 * t16
      t555 = -0.72D2 * t79 * t35 + 0.3096D4 * t151 * t72 * t166 * t184 *
     # t43 + 0.6192D4 * t468 * t110 * t314 - 0.2808D4 * t406 * t86 * t20
     #7 - 0.2808D4 * t406 * t86 * t227 - 0.576D3 * t157 * t281 + 0.432D3
     # * t427 * t525 * t485 + 0.432D3 * t427 * t525 * t489 - 0.576D3 * t
     #57 * t72 * t295 * t31 + 0.2016D4 * t39 * t72 * t33 * t314 * t193 -
     # 0.1440D4 * t84 * t294 * t16 + 0.1728D4 * t90 * t544 * t52 + 0.172
     #8D4 * t90 * t544 * t31 - 0.288D3 * t57 * t134 - 0.288D3 * t57 * t1
     #38
      t577 = 0.576D3 * t98 - 0.912D3 * t82 - 0.1728D4 * t400 - 0.424D3 *
     # t77 + 0.576D3 * t112 + 0.640D3 * t229 + 0.640D3 * t225 - 0.1152D4
     # * t103 + 0.640D3 * t494 + 0.640D3 * t502 + 0.780D3 * t458 + 0.780
     #D3 * t454 + 0.780D3 * t55 + 0.780D3 * t354 - 0.424D3 * t404 - 0.91
     #2D3 * t408
      rrgg2qqbarh11J6 = -(0.5D1 * wd * (t124 + t202 + t277 + t340 + t394
     # + t448 + t506 + t555) + 0.4D1 * wd * t577) * nf / t1 / z / 0.3141
     #592653589793D1 / 0.96D2

      end function
  
   
 

      doubleprecision function rrgg2qqbarh11J7
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t7 = 0.1D1 - x1
      t10 = z + x1 * t3
      t11 = 0.1D1 / t10
      t16 = t2 * t4 * t3
      t17 = x1 ** 2
      t19 = x2 * t7
      t23 = t4 ** 2
      t24 = t2 * t23
      t25 = t7 ** 2
      t27 = 0.1D1 - x2
      t30 = z + x1 * t27 * t3
      t31 = t10 ** 2
      t32 = 0.1D1 / t31
      t33 = t30 * t32
      t39 = t2 * t23 * t3
      t40 = x1 * t17
      t42 = x2 ** 2
      t51 = t25 * t7
      t54 = 0.1D1 / t31 / t10
      t59 = t24 * t51 * t30
      t60 = t32 * x2
      t61 = 0.1D1 - x3
      t75 = t24 * t40
      t76 = x3 * t27
      t80 = cos(x4 * 0.3141592653589793D1)
      t84 = Sqrt(t76 * t10 * x2 * t61)
      t86 = 0.2D1 * t80 * t84
      t87 = t76 * t10 + x2 * t61 - t86
      t95 = t61 * t27 * t10 + x2 * x3 + t86
      t100 = t24 * t17
      t101 = t25 * x2
      t111 = t24 * t25 * t30
      t112 = t54 * x2
      t132 = 0.576D3 * t2 * t4 * x2 * x1 * t7 * t11 - 0.912D3 * t16 * t1
     #7 * t19 * t11 - 0.1728D4 * t24 * t25 * t33 * t17 * x2 - 0.424D3 * 
     #t39 * t40 * t42 * t25 * t32 + 0.576D3 * t2 * t23 * t4 * t42 * x2 *
     # t40 * t51 * t54 + 0.640D3 * t59 * t60 * x1 * t61 + 0.640D3 * t59 
     #* t60 * x1 * x3 - 0.1152D4 * t24 * t42 * t17 * t25 * t32 + 0.640D3
     # * t75 * t19 * t32 * t87 + 0.640D3 * t75 * t19 * t32 * t95 + 0.780
     #D3 * t100 * t101 * t11 * t61 + 0.780D3 * t100 * t101 * t11 * x3 + 
     #0.780D3 * t111 * t112 * t17 * t87 + 0.780D3 * t111 * t112 * t17 * 
     #t95 - 0.424D3 * t39 * t51 * t30 * t54 * t42 * t17 - 0.912D3 * t16 
     #* t25 * t33 * x2 * x1
      rrgg2qqbarh11J7 = -0.5D1 / 0.96D2 * wd * t132 * nf / t1 / z / 0.31
     #41592653589793D1

      end function
  
 