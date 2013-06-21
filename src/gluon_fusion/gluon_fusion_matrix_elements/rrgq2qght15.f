  
      subroutine rrgq2qght15
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgq2qght15s1e1  
      doubleprecision rrgq2qght15s1e0  
      doubleprecision rrgq2qght15s1em1  
      doubleprecision rrgq2qght15s1em2  
      doubleprecision rrgq2qght15s1em3  
      doubleprecision rrgq2qght15s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght15s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgq2qght15s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght15s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgq2qght15s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgq2qght15s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgq2qght15s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgq2qght15s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = t1 * x1
      t3 = -0.1D1 + x1
      t4 = -0.1D1 + x4
      t7 = kappaf(-t2 * t3 * t4)
      t8 = s * t7
      t10 = t1 * t3
      t11 = t10 * x4
      t14 = sqrt(x4)
      t15 = -0.1D1 + t14
      t17 = t14 + 0.1D1
      t18 = t3 * t15 * t17
      t20 = t7 ** 2
      t22 = t1 ** 2
      t24 = x1 * t3
      t27 = t7 * x1
      t28 = t27 * z
      t29 = t7 * z
      t30 = x1 * x4
      t31 = t29 * t30
      t32 = t27 * x4
      t34 = 0.1D1 / (-0.1D1 - t28 + t27 + t31 - t32)
      t37 = (-0.1D1 + t28 - t27) ** 2
      t38 = t29 * x4
      t39 = t7 * x4
      t41 = (t31 - t28 - t32 + t27 - t38 + t29 + t39 - t7 - 0.1D1) ** 2
      t45 = 0.1D1 / (-0.2D1 + t7)
      t49 = t20 / (-0.1D1 - t28 + t27 + t39 - t32 - t38 + t31)
      t50 = 0.1D1 / x3
      t55 = x2 * 0.3141592653589793D1
      t56 = sin(t55)
      t57 = t56 ** 2
      t59 = z ** 2
      t60 = 0.1D1 / t59
      t63 = x1 ** 2
      t64 = t3 ** 2
      t66 = t22 ** 2
      t67 = t20 ** 2
      t73 = log(-0.4D1 * t15 * t57 * t60 * t17 * t63 * t64 * t66 * t67 *
     # x4)
      t76 = 0.90D2 * wd
      t78 = 0.180D3 * lh * wd
      t88 = wd * t34 * t37 * t41 * t1 * t45 * t49 * t50 + (-0.90D2 * wd 
     #* t1 * t73 + (t76 - t78) * t1) * t34 * t37 * t41 * t45 * t49 / 0.9
     #0D2
      t89 = FJET(XB1, XB2, s, t8 * t2, -t8 * t11, 0.0D0, t8 * t1 * t18, 
     #s * t20 * t22 * t24 * t4, t88)
      t93 = cos(t55)
      t94 = sqrt(x3)
      t96 = -0.1D1 + t94
      t97 = t94 + 0.1D1
      t98 = t96 * t97
      t101 = Sqrt(t98 * t15 * t17)
      t102 = t14 * t101
      t105 = 0.1D1 - x4 - x3 + 0.2D1 * x3 * x4 + 0.2D1 * t93 * t94 * t10
     #2
      t108 = kappaf(t2 * t3 * t105)
      t109 = s * t108
      t110 = t109 * t1
      t112 = x1 * t96 * t97
      t115 = t2 * x3
      t118 = t108 ** 2
      t123 = t108 * x1
      t124 = t123 * z
      t125 = t108 * z
      t126 = x3 * x1
      t127 = t125 * t126
      t128 = t108 * x3
      t129 = t128 * x1
      t131 = t123 * x4
      t133 = t125 * t30
      t135 = 0.1D1 / (-0.1D1 - t124 + t127 - t129 + t123 + t108 * x4 - t
     #131 - t125 * x4 + t133)
      t137 = t14 * t118
      t138 = x3 * z
      t139 = t138 * x1
      t142 = t14 * x4
      t143 = t142 * t118
      t144 = t138 * t63
      t147 = t59 * x3
      t148 = t147 * x1
      t152 = t108 * t93
      t153 = x1 * t94
      t154 = t153 * t101
      t157 = t108 * t14
      t160 = t147 * t63
      t168 = x4 * t118
      t169 = t168 * z
      t171 = t94 * t101
      t172 = t93 * x1 * t171
      t176 = t93 * t63 * t171
      t179 = t168 * t59
      t184 = t142 * t108
      t188 = -0.2D1 * t137 * t139 - 0.4D1 * t143 * t144 - 0.2D1 * t143 *
     # t148 + t137 * t148 + 0.4D1 * t152 * t154 - 0.3D1 * t157 * t139 - 
     #t137 * t160 + 0.4D1 * t143 * t139 + 0.2D1 * t137 * t144 + 0.2D1 * 
     #t143 * t160 + 0.4D1 * t169 * t172 - 0.4D1 * t169 * t176 - 0.2D1 * 
     #t179 * t172 + 0.2D1 * t179 * t176 - t157 + t184 - t14 - t143 * t63
     # + t143 * x1 - t184 * z
      t195 = x1 * z
      t199 = z * t63
      t202 = t59 * x1
      t204 = t59 * t63
      t208 = x3 * t63
      t220 = t168 * t93
      t223 = t125 * t93
      t230 = t137 * t126 - t137 * t208 - t137 * t202 + t137 * t204 + 0.2
     #D1 * t137 * t195 - 0.2D1 * t137 * t199 + 0.2D1 * t143 * t208 + 0.3
     #D1 * t157 * t126 - 0.2D1 * t220 * t154 - 0.4D1 * t223 * t154 + 0.2
     #D1 * t220 * t63 * t94 * t101
      t233 = (t188 - t184 * x1 + t137 * t63 - t137 * x1 + t125 * t14 - 0
     #.2D1 * t143 * t126 + t184 * t195 - 0.2D1 * t143 * t195 + 0.2D1 * t
     #143 * t199 + t143 * t202 - t143 * t204 + t230) ** 2
      t235 = 0.1D1 / (-0.2D1 + t108)
      t251 = t129 + t124 - t123 + 0.2D1 * t223 * t153 * t102 - 0.2D1 * t
     #152 * x1 * t94 * t14 * t101 - 0.2D1 * t128 * t30 + 0.2D1 * t125 * 
     #t126 * x4 - t133 + t131 - t127 + 0.1D1
      t255 = 0.1D1 / x4
      t257 = 0.1D1 / t251 * t1 * t118 * t50 * t255
      t259 = FJET(XB1, XB2, s, -t110 * t112, -t109 * t11, t109 * t115, t
     #110 * t18, -s * t118 * t22 * t24 * t105, wd * t135 * t233 * t235 *
     # t257)
      t265 = -0.1D1 + x3
      t268 = kappaf(-t2 * t3 * t265)
      t269 = s * t268
      t274 = t268 ** 2
      t279 = t274 ** 2
      t282 = t268 * x1
      t289 = (-0.1D1 - t282 * z + t268 * z * t126 - t268 * x3 * x1 + t28
     #2) ** 2
      t291 = t22 * t1 / t289
      t293 = t93 ** 2
      t294 = t63 * t293
      t295 = Sqrt(-t98)
      t296 = t295 ** 2
      t299 = t296 / (-0.2D1 + t268)
      t311 = log(-0.4D1 * t57 * t60 * t279 * t66 * t208 * t98 * t64)
      t320 = 0.16D2 * wd * t279 * t291 * t294 * t299 * t255 + 0.8D1 / 0.
     #45D2 * (-0.90D2 * wd * t311 + t76 - t78) * t279 * t291 * t294 * t2
     #99
      t321 = FJET(XB1, XB2, s, -t269 * t1 * t112, 0.0D0, t269 * t115, -t
     #269 * t10, s * t274 * t22 * t24 * t265, t320)
      rrgq2qght15s1e1 = t89 * t88 + t259 * wd * t135 * t233 * t235 * t25
     #7 + t321 * t320

      end function



      doubleprecision function rrgq2qght15s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = t1 * x1
      t3 = -0.1D1 + x1
      t4 = -0.1D1 + x3
      t7 = kappaf(-t2 * t3 * t4)
      t8 = s * t7
      t10 = sqrt(x3)
      t11 = -0.1D1 + t10
      t13 = t10 + 0.1D1
      t18 = t1 * t3
      t20 = t7 ** 2
      t22 = t1 ** 2
      t24 = x1 * t3
      t27 = t20 ** 2
      t29 = t22 * t1
      t30 = t7 * x1
      t38 = (-0.1D1 - t30 * z + t7 * z * x3 * x1 - t7 * x3 * x1 + t30) *
     #* 2
      t39 = 0.1D1 / t38
      t42 = x1 ** 2
      t44 = cos(x2 * 0.3141592653589793D1)
      t45 = t44 ** 2
      t48 = Sqrt(-t11 * t13)
      t49 = t48 ** 2
      t51 = 0.1D1 / (-0.2D1 + t7)
      t56 = FJET(XB1, XB2, s, -t8 * t1 * x1 * t11 * t13, 0.0D0, t8 * t2 
     #* x3, -t8 * t18, s * t20 * t22 * t24 * t4, 0.16D2 * wd * t27 * t29
     # * t39 * t42 * t45 * t49 * t51)
      t66 = -0.1D1 + x4
      t69 = kappaf(-t2 * t3 * t66)
      t70 = s * t69
      t75 = sqrt(x4)
      t81 = t69 ** 2
      t87 = t69 * x1
      t88 = t87 * z
      t89 = t69 * z
      t91 = t89 * x1 * x4
      t92 = t87 * x4
      t94 = 0.1D1 / (-0.1D1 - t88 + t87 + t91 - t92)
      t96 = (-0.1D1 + t88 - t87) ** 2
      t99 = t89 * x4
      t100 = t69 * x4
      t102 = (t91 - t88 - t92 + t87 - t99 + t89 + t100 - t69 - 0.1D1) **
     # 2
      t104 = 0.1D1 / (-0.2D1 + t69)
      t107 = 0.1D1 / (-0.1D1 - t88 + t87 + t100 - t92 - t99 + t91)
      t111 = FJET(XB1, XB2, s, t70 * t2, -t70 * t18 * x4, 0.0D0, t70 * t
     #1 * t3 * (-0.1D1 + t75) * (t75 + 0.1D1), s * t81 * t22 * t24 * t66
     #, wd * t1 * t94 * t96 * t102 * t104 * t81 * t107)
      rrgq2qght15s1e0 = 0.16D2 * t56 * wd * t27 * t29 * t39 * t42 * t45 
     #* t49 * t51 + t111 * wd * t1 * t94 * t96 * t102 * t104 * t81 * t10
     #7

      end function



      doubleprecision function rrgq2qght15s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgq2qght15s1em1 = 0.0D0

      end function



      doubleprecision function rrgq2qght15s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgq2qght15s1em2 = 0.0D0

      end function



      doubleprecision function rrgq2qght15s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgq2qght15s1em3 = 0.0D0

      end function



      doubleprecision function rrgq2qght15s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgq2qght15s1em4 = 0.0D0

      end function
