      subroutine rrgg2gght16
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      if(z.eq.1d0)then
         call rrgg2gghsoftt16
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      else
         call rrgg2gghhardt16
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      end if
      end subroutine

  
      subroutine rrgg2gghhardt16
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2gghhardt16s1e1  
      doubleprecision rrgg2gghhardt16s1e0  
      doubleprecision rrgg2gghhardt16s1em1  
      doubleprecision rrgg2gghhardt16s1em2  
      doubleprecision rrgg2gghhardt16s1em3  
      doubleprecision rrgg2gghhardt16s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt16s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt16s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt16s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt16s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt16s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt16s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gghhardt16s1e1
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
      t10 = sqrt(x4)
      t11 = -0.1D1 + t10
      t13 = t10 + 0.1D1
      t14 = t3 * t11 * t13
      t15 = t8 * t1 * t14
      t16 = t8 * t2
      t17 = t1 * t3
      t18 = t17 * x4
      t19 = t8 * t18
      t20 = t7 ** 2
      t22 = t1 ** 2
      t24 = x1 * t3
      t26 = s * t20 * t22 * t24 * t4
      t27 = t7 * x1
      t28 = t7 * z
      t29 = x1 * x4
      t30 = t28 * t29
      t31 = t27 * x4
      t32 = t27 * z
      t34 = 0.1D1 / (t27 - 0.1D1 + t30 - t31 - t32)
      t37 = (-0.1D1 + t32 - t27) ** 2
      t38 = t28 * x4
      t39 = t7 * x4
      t41 = (-t32 + t30 + t27 - t31 + t28 - t38 - t7 + t39 - 0.1D1) ** 2
      t45 = 0.1D1 / (-0.2D1 + t7)
      t47 = 0.1D1 / (-0.1D1 - t32 + t27 - t38 + t30 + t39 - t31)
      t50 = 0.1D1 / x3
      t55 = t22 * wd
      t56 = t20 ** 2
      t58 = t11 * t13
      t60 = t3 ** 2
      t61 = x1 ** 2
      t62 = t60 * t61
      t63 = x2 * pi
      t64 = sin(t63)
      t65 = t64 ** 2
      t66 = z ** 2
      t68 = t65 / t66
      t72 = log(-0.4D1 * t56 * x4 * t58 * t62 * t68)
      t88 = -0.9D1 / 0.8D1 * wd * t34 * t37 * t41 * t45 * t47 * t22 * t2
     #0 * t50 - (-0.90D2 * t55 * t72 + (0.90D2 * t22 - 0.180D3 * t22 * l
     #h) * wd) * t34 * t37 * t41 * t45 * t47 * t20 / 0.80D2
      t89 = FJET(XB1, XB2, s, 0.0D0, t15, t16, -t19, t26, t88)
      t91 = -0.1D1 + x3
      t94 = kappaf(-t2 * t3 * t91)
      t95 = s * t94
      t97 = sqrt(x3)
      t98 = -0.1D1 + t97
      t100 = t97 + 0.1D1
      t101 = x1 * t98 * t100
      t102 = t95 * t1 * t101
      t103 = t95 * t17
      t104 = t2 * x3
      t105 = t95 * t104
      t106 = t94 ** 2
      t110 = s * t106 * t22 * t24 * t91
      t111 = cos(t63)
      t112 = t111 ** 2
      t114 = t98 * t100
      t115 = Sqrt(-t114)
      t116 = t115 ** 2
      t117 = t61 * t116
      t119 = t106 ** 2
      t120 = t22 ** 2
      t121 = t119 * t120
      t122 = t94 * x1
      t127 = x3 * x1
      t130 = (-0.1D1 - t122 * z + t122 - t94 * x3 * x1 + t94 * z * t127)
     # ** 2
      t134 = 0.1D1 / t130 / (-0.2D1 + t94)
      t135 = 0.1D1 / x4
      t145 = log(-0.4D1 * t119 * x3 * t62 * t114 * t68)
      t157 = -0.18D2 * wd * t112 * t117 * t121 * t134 * t135 - (-0.90D2 
     #* wd * t145 + (0.90D2 - 0.180D3 * lh) * wd) * t112 * t117 * t121 *
     # t134 / 0.5D1
      t158 = FJET(XB1, XB2, s, 0.0D0, -t102, -t103, t105, t110, t157)
      t160 = FJET(XB1, XB2, s, t16, -t19, 0.0D0, t15, t26, t88)
      t166 = Sqrt(t114 * t58)
      t167 = t10 * t166
      t170 = 0.1D1 - x4 - x3 + 0.2D1 * x3 * x4 + 0.2D1 * t111 * t97 * t1
     #67
      t173 = kappaf(t2 * t3 * t170)
      t174 = s * t173
      t175 = t174 * t104
      t176 = t174 * t1
      t177 = t176 * t14
      t178 = t176 * t101
      t179 = t174 * t18
      t180 = t173 ** 2
      t184 = s * t180 * t22 * t24 * t170
      t185 = x4 * t180
      t186 = t185 * t111
      t191 = x1 * t97
      t192 = t191 * t166
      t195 = t173 * z
      t196 = t195 * t111
      t200 = t10 * x4
      t201 = t180 * t200
      t203 = t200 * t173
      t206 = t10 * t180
      t210 = x1 * z
      t213 = t66 * x1
      t215 = z * t61
      t222 = x3 * t61
      t228 = 0.2D1 * t186 * t61 * t97 * t166 - 0.2D1 * t186 * t192 - 0.4
     #D1 * t196 * t192 - t10 + t195 * t10 + t201 * x1 - t203 * x1 - t201
     # * t61 + t206 * t61 - t206 * x1 - t203 * z - 0.2D1 * t201 * t210 +
     # t201 * t213 + 0.2D1 * t201 * t215 - t206 * t213 + t206 * t127 - 0
     #.2D1 * t206 * t215 + 0.2D1 * t201 * t222 + t203 * t210 + 0.2D1 * t
     #206 * t210
      t230 = t173 * t10
      t233 = t66 * t61
      t238 = t66 * x3
      t239 = t238 * x1
      t242 = x3 * z
      t243 = t242 * x1
      t246 = t238 * t61
      t250 = t173 * t111
      t254 = t242 * t61
      t264 = t185 * z
      t266 = t97 * t166
      t267 = t111 * x1 * t266
      t271 = t111 * t61 * t266
      t274 = t185 * t66
      t279 = 0.4D1 * t250 * t192 - t206 * t246 + 0.2D1 * t206 * t254 - 0
     #.4D1 * t201 * t254 - 0.3D1 * t230 * t243 - 0.2D1 * t206 * t243 + t
     #206 * t239 + 0.4D1 * t264 * t267 - 0.4D1 * t264 * t271 - 0.2D1 * t
     #274 * t267 + 0.2D1 * t274 * t271
      t282 = (t228 - t206 * t222 + 0.3D1 * t230 * t127 + t206 * t233 - 0
     #.2D1 * t201 * t127 - t201 * t233 - t230 + t203 - 0.2D1 * t201 * t2
     #39 + 0.4D1 * t201 * t243 + 0.2D1 * t201 * t246 + t279) ** 2
      t283 = t173 * x1
      t284 = t173 * x3
      t287 = t195 * t29
      t288 = t195 * t127
      t292 = t283 * x4
      t298 = t284 * x1
      t302 = t283 * z
      t303 = -t283 - 0.2D1 * t284 * t29 - t287 - t288 + 0.2D1 * t196 * t
     #191 * t167 + t292 - 0.2D1 * t250 * x1 * t97 * t10 * t166 + t298 + 
     #0.2D1 * t195 * t127 * x4 + t302 + 0.1D1
      t304 = 0.1D1 / t303
      t316 = 0.1D1 / (-0.2D1 + t173) / (-0.1D1 - t302 + t283 - t298 + t2
     #88 - t195 * x4 + t287 + t173 * x4 - t292) * t180 * t50 * t135
      t318 = 0.9D1 / 0.8D1 * t55 * t282 * t304 * t316
      t319 = FJET(XB1, XB2, s, t175, t177, -t178, -t179, -t184, -t318)
      t322 = t22 * t282 * t304
      t326 = FJET(XB1, XB2, s, t105, -t103, -t102, 0.0D0, t110, t157)
      t328 = FJET(XB1, XB2, s, t177, t175, -t179, -t178, -t184, -t318)
      t333 = FJET(XB1, XB2, s, t15, 0.0D0, -t19, t16, t26, t88)
      t335 = FJET(XB1, XB2, s, -t103, t105, 0.0D0, -t102, t110, t157)
      t337 = FJET(XB1, XB2, s, -t179, -t178, t177, t175, -t184, -t318)
      t342 = FJET(XB1, XB2, s, -t19, t16, t15, 0.0D0, t26, t88)
      t344 = FJET(XB1, XB2, s, -t178, -t179, t175, t177, -t184, -t318)
      t349 = FJET(XB1, XB2, s, -t102, 0.0D0, t105, -t103, t110, t157)
      rrgg2gghhardt16s1e1 = t89 * t88 + t158 * t157 + t160 * t88 - 0.9D1
     # / 0.8D1 * t319 * wd * t322 * t316 + t326 * t157 - 0.9D1 / 0.8D1 *
     # t328 * wd * t322 * t316 + t333 * t88 + t335 * t157 - 0.9D1 / 0.8D
     #1 * t337 * wd * t322 * t316 + t342 * t88 - 0.9D1 / 0.8D1 * t344 * 
     #wd * t322 * t316 + t349 * t157

      end function



      doubleprecision function rrgg2gghhardt16s1e0
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
      t10 = sqrt(x4)
      t15 = t8 * t1 * t3 * (-0.1D1 + t10) * (t10 + 0.1D1)
      t16 = t8 * t2
      t17 = t1 * t3
      t19 = t8 * t17 * x4
      t20 = t7 ** 2
      t22 = t1 ** 2
      t24 = x1 * t3
      t26 = s * t20 * t22 * t24 * t4
      t28 = t7 * x1
      t29 = t7 * z
      t31 = t29 * x1 * x4
      t32 = t28 * x4
      t33 = t28 * z
      t35 = 0.1D1 / (t28 - 0.1D1 + t31 - t32 - t33)
      t37 = (-0.1D1 + t33 - t28) ** 2
      t40 = t29 * x4
      t41 = t7 * x4
      t43 = (-t33 + t31 + t28 - t32 + t29 - t40 - t7 + t41 - 0.1D1) ** 2
      t45 = 0.1D1 / (-0.2D1 + t7)
      t48 = 0.1D1 / (-0.1D1 - t33 + t28 - t40 + t31 + t41 - t32)
      t52 = 0.9D1 / 0.8D1 * t22 * wd * t35 * t37 * t43 * t45 * t48 * t20
      t53 = FJET(XB1, XB2, s, 0.0D0, t15, t16, -t19, t26, -t52)
      t55 = wd * t35
      t60 = t37 * t43 * t45 * t48 * t20
      t63 = -0.1D1 + x3
      t66 = kappaf(-t2 * t3 * t63)
      t67 = s * t66
      t69 = sqrt(x3)
      t70 = -0.1D1 + t69
      t72 = t69 + 0.1D1
      t74 = t67 * t1 * x1 * t70 * t72
      t75 = t67 * t17
      t77 = t67 * t2 * x3
      t78 = t66 ** 2
      t82 = s * t78 * t22 * t24 * t63
      t84 = cos(x2 * pi)
      t85 = t84 ** 2
      t87 = x1 ** 2
      t89 = Sqrt(-t70 * t72)
      t90 = t89 ** 2
      t93 = t78 ** 2
      t94 = t22 ** 2
      t96 = t66 * x1
      t104 = (-0.1D1 - t96 * z + t96 - t66 * x3 * x1 + t66 * z * x3 * x1
     #) ** 2
      t105 = 0.1D1 / t104
      t107 = 0.1D1 / (-0.2D1 + t66)
      t111 = 0.18D2 * wd * t85 * t87 * t90 * t93 * t94 * t105 * t107
      t112 = FJET(XB1, XB2, s, 0.0D0, -t74, -t75, t77, t82, -t111)
      t114 = t85 * t87
      t119 = t90 * t93 * t94 * t105 * t107
      t122 = FJET(XB1, XB2, s, t16, -t19, 0.0D0, t15, t26, -t52)
      t127 = FJET(XB1, XB2, s, t77, -t75, -t74, 0.0D0, t82, -t111)
      t132 = FJET(XB1, XB2, s, t15, 0.0D0, -t19, t16, t26, -t52)
      t137 = FJET(XB1, XB2, s, -t75, t77, 0.0D0, -t74, t82, -t111)
      t142 = FJET(XB1, XB2, s, -t74, 0.0D0, t77, -t75, t82, -t111)
      t147 = FJET(XB1, XB2, s, -t19, t16, t15, 0.0D0, t26, -t52)
      rrgg2gghhardt16s1e0 = -0.9D1 / 0.8D1 * t53 * t22 * t55 * t60 - 0.1
     #8D2 * t112 * wd * t114 * t119 - 0.9D1 / 0.8D1 * t122 * t22 * t55 *
     # t60 - 0.18D2 * t127 * wd * t114 * t119 - 0.9D1 / 0.8D1 * t132 * t
     #22 * t55 * t60 - 0.18D2 * t137 * wd * t114 * t119 - 0.18D2 * t142 
     #* wd * t114 * t119 - 0.9D1 / 0.8D1 * t147 * t22 * t55 * t60

      end function



      doubleprecision function rrgg2gghhardt16s1em1
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
      rrgg2gghhardt16s1em1 = 0.0D0

      end function



      doubleprecision function rrgg2gghhardt16s1em2
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
      rrgg2gghhardt16s1em2 = 0.0D0

      end function



      doubleprecision function rrgg2gghhardt16s1em3
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
      rrgg2gghhardt16s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2gghhardt16s1em4
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
      rrgg2gghhardt16s1em4 = 0.0D0

      end function
  
      subroutine rrgg2gghsoftt16
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2gghsoftt16s1e1  
      doubleprecision rrgg2gghsoftt16s1e0  
      doubleprecision rrgg2gghsoftt16s1em1  
      doubleprecision rrgg2gghsoftt16s1em2  
      doubleprecision rrgg2gghsoftt16s1em3  
      doubleprecision rrgg2gghsoftt16s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt16s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt16s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt16s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt16s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt16s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt16s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gghsoftt16s1e1
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
      rrgg2gghsoftt16s1e1 = 0.0D0

      end function



      doubleprecision function rrgg2gghsoftt16s1e0
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
      rrgg2gghsoftt16s1e0 = 0.0D0

      end function



      doubleprecision function rrgg2gghsoftt16s1em1
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
      rrgg2gghsoftt16s1em1 = 0.0D0

      end function



      doubleprecision function rrgg2gghsoftt16s1em2
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
      rrgg2gghsoftt16s1em2 = 0.0D0

      end function



      doubleprecision function rrgg2gghsoftt16s1em3
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
      rrgg2gghsoftt16s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2gghsoftt16s1em4
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
      rrgg2gghsoftt16s1em4 = 0.0D0

      end function
