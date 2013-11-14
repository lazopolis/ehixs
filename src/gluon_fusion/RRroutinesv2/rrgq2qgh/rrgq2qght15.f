  
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
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
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
      t29 = 0.1D1 / (-0.2D1 + t7)
      t30 = t7 * x1
      t31 = t30 * z
      t32 = t7 * z
      t33 = t32 * x4
      t34 = x1 * x4
      t35 = t32 * t34
      t36 = t7 * x4
      t37 = t30 * x4
      t39 = 0.1D1 / (-0.1D1 + t30 - t31 - t33 + t35 + t36 - t37)
      t43 = 0.1D1 / (-0.1D1 + t35 - t37 - t31 + t30)
      t46 = (-0.1D1 + t31 - t30) ** 2
      t48 = (t35 - t31 - t37 + t30 - t33 + t32 + t36 - t7 - 0.1D1) ** 2
      t49 = t46 * t48
      t50 = 0.1D1 / x3
      t54 = wd * t1
      t55 = t20 ** 2
      t56 = z ** 2
      t57 = 0.1D1 / t56
      t59 = t22 ** 2
      t60 = t3 ** 2
      t63 = x1 ** 2
      t65 = x2 * pi
      t66 = sin(t65)
      t67 = t66 ** 2
      t73 = log(-0.4D1 * t55 * t57 * t59 * t60 * t63 * t15 * t67 * t17 *
     # x4)
      t86 = wd * t20 * t29 * t39 * t1 * t43 * t49 * t50 + (-0.90D2 * t54
     # * t73 + 0.90D2 * t54 - 0.180D3 * t54 * lh) * t20 * t29 * t39 * t4
     #3 * t49 / 0.90D2
      t87 = FJET(XB1, XB2, s, t8 * t2, -t8 * t11, 0.0D0, t8 * t1 * t18, 
     #s * t20 * t22 * t24 * t4, t86)
      t91 = cos(t65)
      t92 = sqrt(x3)
      t94 = -0.1D1 + t92
      t95 = t92 + 0.1D1
      t96 = t94 * t95
      t99 = Sqrt(t96 * t15 * t17)
      t100 = t14 * t99
      t103 = 0.1D1 - x4 - x3 + 0.2D1 * x3 * x4 + 0.2D1 * t91 * t92 * t10
     #0
      t106 = kappaf(t2 * t3 * t103)
      t107 = s * t106
      t108 = t107 * t1
      t110 = x1 * t94 * t95
      t113 = t2 * x3
      t116 = t106 ** 2
      t121 = t106 * x3
      t124 = t106 * z
      t125 = t124 * t34
      t126 = x3 * x1
      t127 = t124 * t126
      t128 = t106 * x1
      t129 = t128 * x4
      t130 = t124 * t91
      t131 = x1 * t92
      t135 = t121 * x1
      t136 = t106 * t91
      t142 = t128 * z
      t146 = -0.2D1 * t121 * t34 - t125 - t127 + t129 + 0.2D1 * t130 * t
     #131 * t100 + t135 - 0.2D1 * t136 * x1 * t92 * t14 * t99 + t142 + 0
     #.2D1 * t124 * t126 * x4 - t128 + 0.1D1
      t147 = 0.1D1 / t146
      t157 = t14 * t116
      t161 = t14 * x4
      t162 = t116 * t161
      t165 = t161 * t106
      t168 = x4 * t116
      t169 = t168 * t56
      t171 = t92 * t99
      t172 = t91 * t63 * t171
      t175 = t168 * z
      t177 = t91 * x1 * t171
      t184 = x3 * t63
      t187 = t56 * t63
      t190 = t106 * t14
      t193 = z * t63
      t196 = x1 * z
      t200 = t56 * x1
      t204 = t157 * t63 - t157 * x1 + t124 * t14 + t162 * x1 - t162 * t6
     #3 - t165 * x1 - t165 * z + 0.2D1 * t169 * t172 + 0.4D1 * t175 * t1
     #77 - 0.4D1 * t175 * t172 - 0.2D1 * t169 * t177 + 0.2D1 * t162 * t1
     #84 + t157 * t187 + t157 * t126 + 0.3D1 * t190 * t126 - 0.2D1 * t15
     #7 * t193 + 0.2D1 * t157 * t196 - t157 * t184 - t157 * t200 - 0.2D1
     # * t162 * t196
      t212 = t168 * t91
      t213 = t131 * t99
      t225 = t56 * x3
      t226 = t225 * t63
      t229 = x3 * z
      t230 = t229 * t63
      t233 = t225 * x1
      t236 = t229 * x1
      t247 = -t190 + 0.4D1 * t136 * t213 + 0.2D1 * t162 * t226 - 0.4D1 *
     # t162 * t230 - 0.2D1 * t162 * t233 + 0.4D1 * t162 * t236 - t157 * 
     #t226 - 0.3D1 * t190 * t236 + t157 * t233 - 0.2D1 * t157 * t236 + 0
     #.2D1 * t157 * t230
      t250 = (t204 - t162 * t187 + 0.2D1 * t162 * t193 + t162 * t200 + t
     #165 * t196 - 0.2D1 * t162 * t126 - t14 - 0.2D1 * t212 * t213 + 0.2
     #D1 * t212 * t63 * t92 * t99 - 0.4D1 * t130 * t213 + t165 + t247) *
     #* 2
      t252 = 0.1D1 / x4
      t254 = 0.1D1 / (-0.2D1 + t106) / (-0.1D1 + t128 - t135 - t142 + t1
     #27 - t124 * x4 + t125 + t106 * x4 - t129) * t250 * t50 * t252
      t256 = FJET(XB1, XB2, s, -t108 * t110, -t107 * t11, t107 * t113, t
     #108 * t18, -s * t116 * t22 * t24 * t103, t54 * t116 * t147 * t254)
      t262 = -0.1D1 + x3
      t265 = kappaf(-t2 * t3 * t262)
      t266 = s * t265
      t271 = t265 ** 2
      t276 = t91 ** 2
      t278 = Sqrt(-t96)
      t279 = t278 ** 2
      t280 = t265 * x1
      t287 = (-0.1D1 + t280 - t265 * x3 * x1 - t280 * z + t265 * z * t12
     #6) ** 2
      t289 = t279 / t287
      t293 = 0.1D1 / (-0.2D1 + t265) * t63
      t295 = t271 ** 2
      t296 = t22 * t1 * t295
      t310 = log(-0.4D1 * x3 * t94 * t95 * t57 * t60 * t67 * t63 * t59 *
     # t295)
      t322 = 0.16D2 * wd * t276 * t289 * t293 * t296 * t252 + 0.8D1 / 0.
     #45D2 * (-0.90D2 * wd * t310 + 0.90D2 * wd - 0.180D3 * wd * lh) * t
     #276 * t289 * t293 * t296
      t323 = FJET(XB1, XB2, s, -t266 * t1 * t110, 0.0D0, t266 * t113, -t
     #266 * t10, s * t271 * t22 * t24 * t262, t322)
      rrgq2qght15s1e1 = t87 * t86 + t256 * wd * t1 * t116 * t147 * t254 
     #+ t323 * t322

      end function



      doubleprecision function rrgq2qght15s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
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
      t28 = cos(x2 * pi)
      t29 = t28 ** 2
      t32 = Sqrt(-t11 * t13)
      t33 = t32 ** 2
      t34 = t7 * x1
      t42 = (-0.1D1 + t34 - t7 * x3 * x1 - t34 * z + t7 * z * x3 * x1) *
     #* 2
      t43 = 0.1D1 / t42
      t47 = 0.1D1 / (-0.2D1 + t7)
      t48 = x1 ** 2
      t50 = t22 * t1
      t51 = t20 ** 2
      t56 = FJET(XB1, XB2, s, -t8 * t1 * x1 * t11 * t13, 0.0D0, t8 * t2 
     #* x3, -t8 * t18, s * t20 * t22 * t24 * t4, 0.16D2 * wd * t29 * t33
     # * t43 * t47 * t48 * t50 * t51)
      t66 = -0.1D1 + x4
      t69 = kappaf(-t2 * t3 * t66)
      t70 = s * t69
      t75 = sqrt(x4)
      t81 = t69 ** 2
      t88 = 0.1D1 / (-0.2D1 + t69)
      t91 = t69 * x1
      t92 = t91 * z
      t93 = t69 * z
      t94 = t93 * x4
      t96 = t93 * x1 * x4
      t97 = t69 * x4
      t98 = t91 * x4
      t100 = 0.1D1 / (-0.1D1 + t91 - t92 - t94 + t96 + t97 - t98)
      t102 = 0.1D1 / (-0.1D1 + t96 - t98 - t92 + t91)
      t105 = (-0.1D1 + t92 - t91) ** 2
      t107 = (t96 - t92 - t98 + t91 - t94 + t93 + t97 - t69 - 0.1D1) ** 
     #2
      t111 = FJET(XB1, XB2, s, t70 * t2, -t70 * t18 * x4, 0.0D0, t70 * t
     #1 * t3 * (-0.1D1 + t75) * (t75 + 0.1D1), s * t81 * t22 * t24 * t66
     #, wd * t1 * t81 * t88 * t100 * t102 * t105 * t107)
      rrgq2qght15s1e0 = 0.16D2 * t56 * wd * t29 * t33 * t43 * t47 * t48 
     #* t50 * t51 + t111 * wd * t1 * t81 * t88 * t100 * t102 * t105 * t1
     #07

      end function



      doubleprecision function rrgq2qght15s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgq2qght15s1em1 = 0.0D0

      end function



      doubleprecision function rrgq2qght15s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgq2qght15s1em2 = 0.0D0

      end function



      doubleprecision function rrgq2qght15s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgq2qght15s1em3 = 0.0D0

      end function



      doubleprecision function rrgq2qght15s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgq2qght15s1em4 = 0.0D0

      end function
