  
      subroutine rrqg2qght12
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrqg2qght12s1e1  
      doubleprecision rrqg2qght12s1e0  
      doubleprecision rrqg2qght12s1em1  
      doubleprecision rrqg2qght12s1em2  
      doubleprecision rrqg2qght12s1em3  
      doubleprecision rrqg2qght12s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrqg2qght12s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrqg2qght12s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrqg2qght12s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrqg2qght12s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrqg2qght12s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrqg2qght12s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrqg2qght12s1e1
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
      t9 = -0.1D1 + x3
      t12 = Sqrt(x2 * t7 * x3 * t9)
      t14 = 0.2D1 * t6 * t12
      t19 = z * wd
      t20 = x2 * z
      t22 = (0.1D1 + t20 - x2) ** 2
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
      t34 = t32 * t9 * t7
      t37 = log(0.4D1 * t27 * t34)
      t38 = t37 ** 2
      t40 = -x2 + t20 - 0.1D1 + x3
      t45 = 0.180D3 * t19 * lh
      t47 = -t45 - 0.90D2 * t19
      t48 = t47 * t23
      t52 = lh ** 2
      t54 = 0.3141592653589793D1 ** 2
      t56 = 0.180D3 * t52 - 0.30D2 * t54
      t58 = t45 + t19 * t56
      t59 = t58 * t23
      t63 = 0.1D1 / x3
      t66 = t19 * t1
      t67 = x1 ** 2
      t68 = t67 * t26
      t69 = t3 * t68
      t72 = log(0.4D1 * t69 * t34)
      t77 = t47 * t1
      t82 = 0.1D1 / x1
      t85 = -(0.45D2 * t24 * t1 * t38 * t40 - t48 * t1 * t37 * t40 + t59
     # * t1 * t40) * t63 / 0.810D3 - (-0.90D2 * t66 * t72 * t23 * t40 + 
     #t77 * t23 * t40) * t63 * t82 / 0.405D3
      t86 = FJET(XB1, XB2, s, 0.0D0, t2 * (0.1D1 - x2 - x3 + t4 + t14), 
     #0.0D0, -t2 * (-x3 + t4 - x2 + t14), 0.0D0, t85)
      t90 = t32 * t7
      t93 = log(-0.4D1 * t27 * t90)
      t94 = t93 ** 2
      t96 = -x2 + t20 - 0.1D1
      t104 = t59 * t1 * t96
      t111 = log(-0.4D1 * x2 * t26 * t90)
      t113 = t111 * z * wd
      t114 = t111 ** 2
      t117 = t114 * z * wd / 0.2D1
      t144 = t26 * t29
      t145 = t31 * t7
      t149 = log(-0.4D1 * t3 * t67 * t144 * t145)
      t160 = x2 * t67
      t164 = log(-0.4D1 * t160 * t26 * t90)
      t165 = t164 ** 2
      t176 = -(-0.45D2 * t24 * t1 * t94 * t96 + t48 * t1 * t93 * t96 - t
     #104) * t63 / 0.810D3 + (-0.180D3 * (t113 + t117) * t96 * lh + t19 
     #* t96 * (0.60D2 * lh * t54 - 0.2884936567583026D3 - 0.120D3 * t52 
     #* lh) + 0.90D2 * (-t117 - t114 * t111 * z * wd / 0.6D1) * t96 + (-
     #t19 - t113) * t96 * t56) * t23 * t1 / 0.810D3 - (0.90D2 * t66 * t1
     #49 * t96 * t23 - t77 * t96 * t23) * t63 * t82 / 0.405D3 + (0.45D2 
     #* t66 * t165 * t96 * t23 - t77 * t164 * t96 * t23 + t104) * t82 / 
     #0.405D3
      t177 = FJET(XB1, XB2, s, 0.0D0, -t2 * t7, 0.0D0, t2 * x2, 0.0D0, t
     #176)
      t181 = -0.1D1 + x1
      t185 = x1 * z
      t186 = 0.1D1 - x1 + t185
      t187 = 0.1D1 / t186
      t194 = s * t30 * x2 * x1 * t181 * t187
      t195 = t181 ** 2
      t196 = t187 * t195
      t201 = log(-0.4D1 * t69 * t32 * t196 * t7)
      t202 = x2 * x1
      t203 = t202 * z
      t204 = x2 - t202 - t20 + t203 + 0.1D1 - x1 + t185
      t207 = (-0.1D1 + x1 - t20 + t203 + x2 - t202 - t185) ** 2
      t208 = 0.1D1 / t207
      t209 = t181 * t208
      t214 = t204 * t181 * t208
      t223 = log(-0.4D1 * t160 * t144 * t145 * t196)
      t224 = t223 ** 2
      t236 = -(-0.90D2 * t66 * t201 * t204 * t209 + t77 * t214) * t63 * 
     #t82 / 0.405D3 + (-0.45D2 * t66 * t224 * t204 * t209 + t77 * t223 *
     # t214 - t58 * t1 * t214) * t82 / 0.405D3
      t237 = FJET(XB1, XB2, s, t2 * x1, t7 * s * t1 * t181, 0.0D0, -t2 *
     # t181 * x2 * t187, -t194, t236)
      t242 = x3 * x1
      t243 = t242 * z
      t244 = t3 * x1
      t245 = t3 * t185
      t250 = Sqrt(x3 * t7 * t186 * x2 * t9)
      t252 = 0.2D1 * t6 * t250
      t253 = 0.1D1 - x1 + t185 - x2 + t202 - t203 - x3 + t242 - t243 + t
     #4 - t244 + t245 + t252
      t270 = log(0.4D1 * t3 * t68 * t29 * t31 * t187 * t195 * t7 * t9)
      t272 = x2 - t202 - t20 + t203 + 0.1D1 - x1 + t185 - x3 + t242 - t2
     #43
      t280 = 0.90D2 * t66 * t270 * t181 * t272 * t208 - t77 * t181 * t27
     #2 * t208
      t284 = FJET(XB1, XB2, s, -t9 * s * t1 * x1, -t2 * t181 * t253 * t1
     #87, t2 * t242, t2 * t181 * (-x3 + t242 - t243 + t4 - t244 + t245 -
     # x2 + t252) * t187, -t194, -t280 * t63 * t82 / 0.405D3)
      rrqg2qght12s1e1 = t86 * t85 + t177 * t176 + t237 * t236 - t284 * t
     #280 * t63 * t82 / 0.405D3

      end function



      doubleprecision function rrqg2qght12s1e0
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
      t9 = -0.1D1 + x3
      t12 = Sqrt(x2 * t7 * x3 * t9)
      t14 = 0.2D1 * t6 * t12
      t19 = z * wd
      t20 = x2 * z
      t22 = (0.1D1 + t20 - x2) ** 2
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
      t37 = log(0.4D1 * t27 * t32 * t9 * t7)
      t39 = -x2 + t20 - 0.1D1 + x3
      t47 = (-0.180D3 * t19 * lh - 0.90D2 * t19) * t1
      t48 = t23 * t39
      t51 = 0.1D1 / x3
      t54 = t19 * t1
      t55 = 0.1D1 / x1
      t56 = t51 * t55
      t60 = -(-0.90D2 * t24 * t1 * t37 * t39 + t47 * t48) * t51 / 0.810D
     #3 - 0.2D1 / 0.9D1 * t54 * t48 * t56
      t61 = FJET(XB1, XB2, s, 0.0D0, t2 * (0.1D1 - x2 - x3 + t4 + t14), 
     #0.0D0, -t2 * (-x3 + t4 - x2 + t14), 0.0D0, t60)
      t65 = t32 * t7
      t68 = log(-0.4D1 * t27 * t65)
      t70 = -x2 + t20 - 0.1D1
      t74 = t70 * t23
      t75 = t47 * t74
      t82 = x1 ** 2
      t83 = x2 * t82
      t87 = log(-0.4D1 * t83 * t26 * t65)
      t98 = log(-0.4D1 * x2 * t26 * t65)
      t100 = t98 * z * wd
      t105 = t98 ** 2
      t112 = lh ** 2
      t114 = 0.3141592653589793D1 ** 2
      t123 = -(0.90D2 * t24 * t1 * t68 * t70 - t75) * t51 / 0.810D3 + 0.
     #2D1 / 0.9D1 * t54 * t74 * t56 + (-0.90D2 * t54 * t87 * t70 * t23 +
     # t75) * t55 / 0.405D3 + (-0.180D3 * (-t19 - t100) * t70 * lh + 0.9
     #0D2 * (t100 + t105 * z * wd / 0.2D1) * t70 + t19 * t70 * (0.180D3 
     #* t112 - 0.30D2 * t114)) * t23 * t1 / 0.810D3
      t124 = FJET(XB1, XB2, s, 0.0D0, -t2 * t7, 0.0D0, t2 * x2, 0.0D0, t
     #123)
      t128 = -0.1D1 + x1
      t129 = t1 * t128
      t132 = x1 * z
      t133 = 0.1D1 - x1 + t132
      t134 = 0.1D1 / t133
      t141 = s * t30 * x2 * x1 * t128 * t134
      t142 = x2 * x1
      t143 = t142 * z
      t144 = x2 - t142 - t20 + t143 + 0.1D1 - x1 + t132
      t148 = (-0.1D1 + x1 - t20 + t143 + x2 - t142 - t132) ** 2
      t149 = 0.1D1 / t148
      t150 = t128 * t149
      t157 = t128 ** 2
      t162 = log(-0.4D1 * t83 * t26 * t29 * t31 * t7 * t157 * t134)
      t173 = -0.2D1 / 0.9D1 * t19 * t1 * t144 * t150 * t56 + (0.90D2 * t
     #54 * t162 * t144 * t150 - t47 * t144 * t128 * t149) * t55 / 0.405D
     #3
      t174 = FJET(XB1, XB2, s, t2 * x1, t7 * s * t129, 0.0D0, -t2 * t128
     # * x2 * t134, -t141, t173)
      t179 = x3 * x1
      t180 = t179 * z
      t181 = t3 * x1
      t182 = t3 * t132
      t187 = Sqrt(x3 * t7 * t133 * x2 * t9)
      t189 = 0.2D1 * t6 * t187
      t190 = 0.1D1 - x1 + t132 - x2 + t142 - t143 - x3 + t179 - t180 + t
     #4 - t181 + t182 + t189
      t200 = x2 - t142 - t20 + t143 + 0.1D1 - x1 + t132 - x3 + t179 - t1
     #80
      t205 = FJET(XB1, XB2, s, -t9 * s * t1 * x1, -t2 * t128 * t190 * t1
     #34, t2 * t179, t2 * t128 * (-x3 + t179 - t180 + t4 - t181 + t182 -
     # x2 + t189) * t134, -t141, 0.2D1 / 0.9D1 * t19 * t129 * t200 * t14
     #9 * t56)
      rrqg2qght12s1e0 = t61 * t60 + t124 * t123 + t174 * t173 + 0.2D1 / 
     #0.9D1 * t205 * z * wd * t1 * t128 * t200 * t149 * t51 * t55

      end function



      doubleprecision function rrqg2qght12s1em1
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
      t12 = Sqrt(x2 * t7 * x3 * (-0.1D1 + x3))
      t14 = 0.2D1 * t6 * t12
      t19 = z * wd
      t20 = t19 * t1
      t21 = x2 * z
      t23 = (0.1D1 + t21 - x2) ** 2
      t24 = 0.1D1 / t23
      t25 = -x2 + t21 - 0.1D1 + x3
      t27 = 0.1D1 / x3
      t31 = FJET(XB1, XB2, s, 0.0D0, t2 * (0.1D1 - x2 - x3 + t4 + t14), 
     #0.0D0, -t2 * (-x3 + t4 - x2 + t14), 0.0D0, -t20 * t24 * t25 * t27 
     #/ 0.9D1)
      t41 = -x2 + t21 - 0.1D1
      t42 = t41 * t24
      t43 = 0.1D1 / x1
      t50 = Sin(t5)
      t51 = t50 ** 2
      t53 = z ** 2
      t55 = t1 ** 2
      t56 = t55 ** 2
      t61 = log(-0.4D1 * x2 * t51 / t53 * t56 * t7)
      t74 = 0.2D1 / 0.9D1 * t20 * t42 * t43 + (-0.180D3 * t19 * t41 * lh
     # + 0.90D2 * (-t19 - t61 * z * wd) * t41) * t24 * t1 / 0.810D3 + t2
     #0 * t42 * t27 / 0.9D1
      t75 = FJET(XB1, XB2, s, 0.0D0, -t2 * t7, 0.0D0, t2 * x2, 0.0D0, t7
     #4)
      t79 = -0.1D1 + x1
      t83 = x1 * z
      t85 = 0.1D1 / (0.1D1 - x1 + t83)
      t93 = x2 * x1
      t94 = t93 * z
      t98 = (-0.1D1 + x1 - t21 + t94 + x2 - t93 - t83) ** 2
      t101 = (x2 - t93 - t21 + t94 + 0.1D1 - x1 + t83) * t79 / t98 * t43
      t104 = FJET(XB1, XB2, s, t2 * x1, t7 * s * t1 * t79, 0.0D0, -t2 * 
     #t79 * x2 * t85, -s * t55 * x2 * x1 * t79 * t85, -0.2D1 / 0.9D1 * t
     #20 * t101)
      rrqg2qght12s1em1 = -t31 * z * wd * t1 * t24 * t25 * t27 / 0.9D1 + 
     #t75 * t74 - 0.2D1 / 0.9D1 * t104 * z * wd * t1 * t101

      end function



      doubleprecision function rrqg2qght12s1em2
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
      t11 = (0.1D1 + t7 - x2) ** 2
      t13 = t1 * (-x2 + t7 - 0.1D1) / t11
      t16 = FJET(XB1, XB2, s, 0.0D0, -t2 * (-0.1D1 + x2), 0.0D0, t2 * x2
     #, 0.0D0, z * wd * t13 / 0.9D1)
      rrqg2qght12s1em2 = t16 * z * wd * t13 / 0.9D1

      end function



      doubleprecision function rrqg2qght12s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrqg2qght12s1em3 = 0.0D0

      end function



      doubleprecision function rrqg2qght12s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrqg2qght12s1em4 = 0.0D0

      end function
