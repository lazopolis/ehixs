  
      subroutine rrqg2qght15
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrqg2qght15s1e1  
      doubleprecision rrqg2qght15s1e0  
      doubleprecision rrqg2qght15s1em1  
      doubleprecision rrqg2qght15s1em2  
      doubleprecision rrqg2qght15s1em3  
      doubleprecision rrqg2qght15s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrqg2qght15s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrqg2qght15s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrqg2qght15s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrqg2qght15s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrqg2qght15s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrqg2qght15s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrqg2qght15s1e1
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
      t14 = x1 * t11 * t13
      t16 = t1 * t3
      t18 = t2 * x3
      t20 = t7 ** 2
      t22 = t1 ** 2
      t24 = t3 * x1
      t27 = t20 ** 2
      t29 = x1 ** 2
      t30 = x2 * pi
      t31 = cos(t30)
      t32 = t31 ** 2
      t33 = t32 * t29
      t35 = t11 * t13
      t36 = Sqrt(-t35)
      t37 = t36 ** 2
      t38 = t7 * x1
      t43 = x3 * x1
      t46 = (-0.1D1 - t38 * z + t38 - t7 * x3 * x1 + t7 * z * t43) ** 2
      t48 = t37 / t46
      t52 = t22 * t1 / (-0.2D1 + t7)
      t53 = 0.1D1 / x4
      t58 = t3 ** 2
      t60 = z ** 2
      t61 = 0.1D1 / t60
      t64 = sin(t30)
      t65 = t64 ** 2
      t67 = t22 ** 2
      t73 = log(-0.4D1 * t58 * t11 * t13 * t61 * t65 * t27 * t29 * t67 *
     # x3)
      t85 = 0.16D2 * wd * t27 * t33 * t48 * t52 * t53 + 0.8D1 / 0.45D2 *
     # (-0.90D2 * wd * t73 + (0.90D2 - 0.180D3 * lh) * wd) * t27 * t33 *
     # t48 * t52
      t86 = FJET(XB1, XB2, s, 0.0D0, -t8 * t1 * t14, -t8 * t16, t8 * t18
     #, s * t20 * t22 * t24 * t4, t85)
      t91 = sqrt(x4)
      t92 = -0.1D1 + t91
      t93 = t91 + 0.1D1
      t96 = Sqrt(t35 * t92 * t93)
      t97 = t91 * t96
      t100 = 0.1D1 - x4 - x3 + 0.2D1 * x3 * x4 + 0.2D1 * t31 * t10 * t97
      t103 = kappaf(t2 * t3 * t100)
      t104 = s * t103
      t105 = t16 * x4
      t107 = t104 * t1
      t110 = t3 * t92 * t93
      t113 = t103 ** 2
      t118 = wd * t1
      t119 = t103 * z
      t120 = t119 * t31
      t121 = x1 * t10
      t122 = t121 * t96
      t125 = x4 * t113
      t126 = t125 * t31
      t133 = t103 * t31
      t136 = t91 * x4
      t137 = t136 * t113
      t138 = t60 * x3
      t139 = t138 * x1
      t142 = x3 * z
      t143 = t142 * x1
      t146 = t138 * t29
      t149 = t142 * t29
      t152 = t91 * t113
      t159 = t103 * t91
      t162 = t125 * t60
      t164 = t10 * t96
      t165 = t31 * t29 * t164
      t168 = t125 * z
      t170 = t31 * x1 * t164
      t177 = t60 * x1
      t179 = x1 * z
      t182 = -0.4D1 * t120 * t122 - 0.2D1 * t126 * t122 + 0.2D1 * t126 *
     # t29 * t10 * t96 + 0.4D1 * t133 * t122 - 0.2D1 * t137 * t139 + 0.4
     #D1 * t137 * t143 + 0.2D1 * t137 * t146 - 0.4D1 * t137 * t149 + t15
     #2 * t139 - t152 * t146 + 0.2D1 * t152 * t149 - 0.2D1 * t152 * t143
     # - 0.3D1 * t159 * t143 + 0.2D1 * t162 * t165 + 0.4D1 * t168 * t170
     # - 0.4D1 * t168 * t165 - 0.2D1 * t162 * t170 - t91 + t137 * t177 -
     # 0.2D1 * t137 * t179
      t183 = z * t29
      t186 = t60 * t29
      t190 = x3 * t29
      t193 = t136 * t103
      t213 = 0.2D1 * t152 * t179 + t152 * t186 + t193 - t159 - t137 * t2
     #9 + t137 * x1 - t193 * z - t193 * x1 - t152 * x1 + t152 * t29 + t1
     #19 * t91
      t216 = (t182 + 0.2D1 * t137 * t183 - t137 * t186 - 0.2D1 * t137 * 
     #t43 + 0.2D1 * t137 * t190 + t193 * t179 - 0.2D1 * t152 * t183 - t1
     #52 * t177 + 0.3D1 * t159 * t43 + t152 * t43 - t152 * t190 + t213) 
     #** 2
      t219 = t103 * x1
      t220 = t219 * z
      t221 = t103 * x3
      t222 = t221 * x1
      t223 = t119 * t43
      t224 = t219 * x4
      t227 = x1 * x4
      t228 = t119 * t227
      t247 = t220 - t223 + t222 - 0.2D1 * t133 * x1 * t10 * t91 * t96 - 
     #0.2D1 * t221 * t227 + 0.2D1 * t119 * t43 * x4 - t228 + 0.2D1 * t12
     #0 * t121 * t97 + t224 - t219 + 0.1D1
      t249 = 0.1D1 / x3
      t252 = 0.1D1 / (-0.1D1 - t220 + t219 - t222 + t223 - t224 + t103 *
     # x4 - t119 * x4 + t228) / (-0.2D1 + t103) / t247 * t249 * t53
      t254 = FJET(XB1, XB2, s, -t104 * t105, -t107 * t14, t107 * t110, t
     #104 * t18, -s * t113 * t22 * t24 * t100, t118 * t113 * t216 * t252
     #)
      t260 = -0.1D1 + x4
      t263 = kappaf(-t2 * t3 * t260)
      t264 = s * t263
      t269 = t263 ** 2
      t274 = t263 * x1
      t275 = t274 * z
      t276 = t263 * z
      t277 = t276 * t227
      t278 = t274 * x4
      t280 = 0.1D1 / (-t275 - 0.1D1 + t277 - t278 + t274)
      t284 = (-0.1D1 + t275 - t274) ** 2
      t285 = t276 * x4
      t286 = t263 * x4
      t288 = (-t275 + t277 + t274 - t278 + t276 - t285 - t263 + t286 - 0
     #.1D1) ** 2
      t289 = t284 * t288
      t294 = 0.1D1 / (-0.1D1 - t275 + t274 - t278 + t286 - t285 + t277) 
     #/ (-0.2D1 + t263)
      t305 = t269 ** 2
      t310 = log(-0.4D1 * x4 * t58 * t93 * t61 * t65 * t67 * t92 * t29 *
     # t305)
      t323 = t118 * t280 * t269 * t289 * t294 * t249 + (-0.90D2 * wd * t
     #1 * t310 + (-0.90D2 + 0.90D2 * z - 0.180D3 * lh * t1) * wd) * t280
     # * t269 * t289 * t294 / 0.90D2
      t324 = FJET(XB1, XB2, s, -t264 * t105, t264 * t2, t264 * t1 * t110
     #, 0.0D0, s * t269 * t22 * t24 * t260, t323)
      rrqg2qght15s1e1 = t86 * t85 + t254 * wd * t1 * t113 * t216 * t252 
     #+ t324 * t323

      end function



      doubleprecision function rrqg2qght15s1e0
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
      t16 = t1 * t3
      t20 = t7 ** 2
      t22 = t1 ** 2
      t24 = t3 * x1
      t27 = t20 ** 2
      t29 = x1 ** 2
      t31 = cos(x2 * pi)
      t32 = t31 ** 2
      t36 = Sqrt(-t11 * t13)
      t37 = t36 ** 2
      t38 = t7 * x1
      t46 = (-0.1D1 - t38 * z + t38 - t7 * x3 * x1 + t7 * z * x3 * x1) *
     #* 2
      t47 = 0.1D1 / t46
      t49 = t22 * t1
      t51 = 0.1D1 / (-0.2D1 + t7)
      t56 = FJET(XB1, XB2, s, 0.0D0, -t8 * t1 * x1 * t11 * t13, -t8 * t1
     #6, t8 * t2 * x3, s * t20 * t22 * t24 * t4, 0.16D2 * wd * t27 * t29
     # * t32 * t37 * t47 * t49 * t51)
      t66 = -0.1D1 + x4
      t69 = kappaf(-t2 * t3 * t66)
      t70 = s * t69
      t75 = sqrt(x4)
      t81 = t69 ** 2
      t86 = 0.90D2 * t1
      t88 = t69 * x1
      t89 = t88 * z
      t90 = t69 * z
      t92 = t90 * x1 * x4
      t93 = t88 * x4
      t95 = 0.1D1 / (-t89 - 0.1D1 + t92 - t93 + t88)
      t99 = (-0.1D1 + t89 - t88) ** 2
      t100 = t90 * x4
      t101 = t69 * x4
      t103 = (-t89 + t92 + t88 - t93 + t90 - t100 - t69 + t101 - 0.1D1) 
     #** 2
      t106 = 0.1D1 / (-0.1D1 - t89 + t88 - t93 + t101 - t100 + t92)
      t108 = 0.1D1 / (-0.2D1 + t69)
      t113 = FJET(XB1, XB2, s, -t70 * t16 * x4, t70 * t2, t70 * t1 * t3 
     #* (-0.1D1 + t75) * (t75 + 0.1D1), 0.0D0, s * t81 * t22 * t24 * t66
     #, t86 * wd * t95 * t81 * t99 * t103 * t106 * t108 / 0.90D2)
      rrqg2qght15s1e0 = 0.16D2 * t56 * wd * t27 * t29 * t32 * t37 * t47 
     #* t49 * t51 + t113 * t86 * wd * t95 * t81 * t99 * t103 * t106 * t1
     #08 / 0.90D2

      end function



      doubleprecision function rrqg2qght15s1em1
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
      rrqg2qght15s1em1 = 0.0D0

      end function



      doubleprecision function rrqg2qght15s1em2
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
      rrqg2qght15s1em2 = 0.0D0

      end function



      doubleprecision function rrqg2qght15s1em3
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
      rrqg2qght15s1em3 = 0.0D0

      end function



      doubleprecision function rrqg2qght15s1em4
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
      rrqg2qght15s1em4 = 0.0D0

      end function
