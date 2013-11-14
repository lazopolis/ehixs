  
      subroutine rrgg2gght16
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2gght16s1e1  
      doubleprecision rrgg2gght16s1e0  
      doubleprecision rrgg2gght16s1em1  
      doubleprecision rrgg2gght16s1em2  
      doubleprecision rrgg2gght16s1em3  
      doubleprecision rrgg2gght16s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght16s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gght16s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght16s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gght16s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gght16s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gght16s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gght16s1e1
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
      t27 = t22 * wd
      t28 = t7 * x1
      t29 = t28 * z
      t30 = t7 * z
      t31 = x1 * x4
      t32 = t30 * t31
      t33 = t28 * x4
      t34 = t30 * x4
      t35 = t7 * x4
      t37 = (-t29 + t32 + t28 - t33 + t30 - t34 - t7 + t35 - 0.1D1) ** 2
      t43 = (-0.1D1 + t29 - t28) ** 2
      t44 = 0.1D1 / (-0.1D1 + t32 - t33 - t29 + t28) * t43
      t49 = 0.1D1 / (-0.1D1 + t28 - t29 + t35 - t33 - t34 + t32) / (-0.2
     #D1 + t7)
      t50 = 0.1D1 / x3
      t55 = x2 * pi
      t56 = sin(t55)
      t57 = t56 ** 2
      t58 = z ** 2
      t59 = 0.1D1 / t58
      t61 = t3 ** 2
      t64 = x1 ** 2
      t66 = t20 ** 2
      t71 = log(-0.4D1 * t57 * t59 * t61 * t11 * t13 * t64 * x4 * t66)
      t83 = -0.9D1 / 0.8D1 * t27 * t20 * t37 * t44 * t49 * t50 - (-0.90D
     #2 * t27 * t71 + 0.90D2 * t27 - 0.180D3 * t27 * lh) * t20 * t37 * t
     #44 * t49 / 0.80D2
      t84 = FJET(XB1, XB2, s, 0.0D0, t15, t16, -t19, t26, t83)
      t86 = -0.1D1 + x3
      t89 = kappaf(-t2 * t3 * t86)
      t90 = s * t89
      t92 = sqrt(x3)
      t93 = -0.1D1 + t92
      t95 = t92 + 0.1D1
      t96 = x1 * t93 * t95
      t97 = t90 * t1 * t96
      t98 = t90 * t17
      t99 = t2 * x3
      t100 = t90 * t99
      t101 = t89 ** 2
      t105 = s * t101 * t22 * t24 * t86
      t106 = t89 * x1
      t108 = x3 * x1
      t114 = (-0.1D1 + t106 + t89 * z * t108 - t106 * z - t89 * x3 * x1)
     # ** 2
      t115 = 0.1D1 / t114
      t117 = cos(t55)
      t118 = t117 ** 2
      t119 = t118 * t64
      t121 = t93 * t95
      t122 = Sqrt(-t121)
      t123 = t122 ** 2
      t124 = t22 ** 2
      t125 = t123 * t124
      t126 = t101 ** 2
      t129 = t126 / (-0.2D1 + t89)
      t130 = 0.1D1 / x4
      t142 = log(-0.4D1 * x3 * t57 * t59 * t61 * t121 * t64 * t126)
      t154 = -0.18D2 * wd * t115 * t119 * t125 * t129 * t130 - (-0.90D2 
     #* wd * t142 + 0.90D2 * wd - 0.180D3 * wd * lh) * t115 * t119 * t12
     #5 * t129 / 0.5D1
      t155 = FJET(XB1, XB2, s, 0.0D0, -t97, -t98, t100, t105, t154)
      t157 = FJET(XB1, XB2, s, t16, -t19, 0.0D0, t15, t26, t83)
      t164 = Sqrt(t121 * t11 * t13)
      t165 = t10 * t164
      t168 = 0.1D1 - x4 - x3 + 0.2D1 * x3 * x4 + 0.2D1 * t117 * t92 * t1
     #65
      t171 = kappaf(t2 * t3 * t168)
      t172 = s * t171
      t173 = t172 * t99
      t174 = t172 * t1
      t175 = t174 * t14
      t176 = t174 * t96
      t177 = t172 * t18
      t178 = t171 ** 2
      t182 = s * t178 * t22 * t24 * t168
      t183 = t171 * x3
      t186 = t171 * z
      t187 = t186 * t31
      t188 = t186 * t108
      t189 = t171 * t117
      t195 = t171 * x1
      t196 = t195 * x4
      t200 = t183 * x1
      t201 = t186 * t117
      t202 = x1 * t92
      t206 = t195 * z
      t207 = -0.2D1 * t183 * t31 - t187 - t188 - 0.2D1 * t189 * x1 * t92
     # * t10 * t164 + t196 + 0.2D1 * t186 * t108 * x4 + t200 + 0.2D1 * t
     #201 * t202 * t165 + t206 - t195 + 0.1D1
      t208 = 0.1D1 / t207
      t218 = x4 * t178
      t219 = t218 * t58
      t221 = t92 * t164
      t222 = t117 * x1 * t221
      t225 = t218 * z
      t229 = t117 * t64 * t221
      t234 = t10 * x4
      t235 = t234 * t178
      t236 = x3 * z
      t237 = t236 * x1
      t240 = t236 * t64
      t243 = t58 * x3
      t244 = t243 * x1
      t247 = t243 * t64
      t250 = t202 * t164
      t253 = t10 * t178
      t260 = t171 * t10
      t263 = t234 * t171
      t266 = t218 * t117
      t273 = -0.2D1 * t219 * t222 + 0.4D1 * t225 * t222 - 0.4D1 * t225 *
     # t229 + 0.2D1 * t219 * t229 + 0.4D1 * t235 * t237 - 0.4D1 * t235 *
     # t240 - 0.2D1 * t235 * t244 + 0.2D1 * t235 * t247 + 0.4D1 * t189 *
     # t250 - t253 * t247 + t253 * t244 - 0.2D1 * t253 * t237 + 0.2D1 * 
     #t253 * t240 - 0.3D1 * t260 * t237 + t263 - t260 - t10 - 0.4D1 * t2
     #01 * t250 + 0.2D1 * t266 * t64 * t92 * t164 - 0.2D1 * t266 * t250
      t275 = x3 * t64
      t278 = x1 * z
      t284 = t58 * x1
      t286 = z * t64
      t289 = t58 * t64
      t308 = 0.2D1 * t235 * t286 - t235 * t289 + t235 * t284 - 0.2D1 * t
     #235 * t108 - t235 * t64 - t263 * x1 + t235 * x1 - t263 * z + t186 
     #* t10 + t253 * t64 - t253 * x1
      t311 = (t273 + t253 * t108 + 0.2D1 * t235 * t275 + t263 * t278 + 0
     #.3D1 * t260 * t108 + 0.2D1 * t253 * t278 - t253 * t284 - 0.2D1 * t
     #253 * t286 + t253 * t289 - t253 * t275 - 0.2D1 * t235 * t278 + t30
     #8) ** 2
      t314 = 0.1D1 / (-0.2D1 + t171) / (-0.1D1 + t195 + t188 - t206 - t2
     #00 + t171 * x4 - t196 - t186 * x4 + t187) * t311 * t50 * t130
      t316 = 0.9D1 / 0.8D1 * t27 * t178 * t208 * t314
      t317 = FJET(XB1, XB2, s, t173, t175, -t176, -t177, -t182, -t316)
      t320 = t22 * t178 * t208
      t324 = FJET(XB1, XB2, s, t100, -t98, -t97, 0.0D0, t105, t154)
      t326 = FJET(XB1, XB2, s, t175, t173, -t177, -t176, -t182, -t316)
      t331 = FJET(XB1, XB2, s, t15, 0.0D0, -t19, t16, t26, t83)
      t333 = FJET(XB1, XB2, s, -t98, t100, 0.0D0, -t97, t105, t154)
      t335 = FJET(XB1, XB2, s, -t177, -t176, t175, t173, -t182, -t316)
      t340 = FJET(XB1, XB2, s, -t19, t16, t15, 0.0D0, t26, t83)
      t342 = FJET(XB1, XB2, s, -t176, -t177, t173, t175, -t182, -t316)
      t347 = FJET(XB1, XB2, s, -t97, 0.0D0, t100, -t98, t105, t154)
      rrgg2gght16s1e1 = t84 * t83 + t155 * t154 + t157 * t83 - 0.9D1 / 0
     #.8D1 * t317 * wd * t320 * t314 + t324 * t154 - 0.9D1 / 0.8D1 * t32
     #6 * wd * t320 * t314 + t331 * t83 + t333 * t154 - 0.9D1 / 0.8D1 * 
     #t335 * wd * t320 * t314 + t340 * t83 - 0.9D1 / 0.8D1 * t342 * wd *
     # t320 * t314 + t347 * t154

      end function



      doubleprecision function rrgg2gght16s1e0
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
      t29 = t28 * z
      t30 = t7 * z
      t32 = t30 * x1 * x4
      t33 = t28 * x4
      t34 = t30 * x4
      t35 = t7 * x4
      t37 = (-t29 + t32 + t28 - t33 + t30 - t34 - t7 + t35 - 0.1D1) ** 2
      t41 = 0.1D1 / (-0.1D1 + t32 - t33 - t29 + t28)
      t43 = (-0.1D1 + t29 - t28) ** 2
      t46 = 0.1D1 / (-0.1D1 + t28 - t29 + t35 - t33 - t34 + t32)
      t48 = 0.1D1 / (-0.2D1 + t7)
      t52 = 0.9D1 / 0.8D1 * t22 * wd * t20 * t37 * t41 * t43 * t46 * t48
      t53 = FJET(XB1, XB2, s, 0.0D0, t15, t16, -t19, t26, -t52)
      t55 = wd * t20
      t60 = t37 * t41 * t43 * t46 * t48
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
      t83 = t66 * x1
      t91 = (-0.1D1 + t83 + t66 * z * x3 * x1 - t83 * z - t66 * x3 * x1)
     # ** 2
      t92 = 0.1D1 / t91
      t95 = cos(x2 * pi)
      t96 = t95 ** 2
      t97 = x1 ** 2
      t101 = Sqrt(-t70 * t72)
      t102 = t101 ** 2
      t103 = t22 ** 2
      t105 = t78 ** 2
      t107 = 0.1D1 / (-0.2D1 + t66)
      t111 = 0.18D2 * wd * t92 * t96 * t97 * t102 * t103 * t105 * t107
      t112 = FJET(XB1, XB2, s, 0.0D0, -t74, -t75, t77, t82, -t111)
      t114 = t92 * t96
      t119 = t97 * t102 * t103 * t105 * t107
      t122 = FJET(XB1, XB2, s, t16, -t19, 0.0D0, t15, t26, -t52)
      t127 = FJET(XB1, XB2, s, t77, -t75, -t74, 0.0D0, t82, -t111)
      t132 = FJET(XB1, XB2, s, t15, 0.0D0, -t19, t16, t26, -t52)
      t137 = FJET(XB1, XB2, s, -t75, t77, 0.0D0, -t74, t82, -t111)
      t142 = FJET(XB1, XB2, s, -t74, 0.0D0, t77, -t75, t82, -t111)
      t147 = FJET(XB1, XB2, s, -t19, t16, t15, 0.0D0, t26, -t52)
      rrgg2gght16s1e0 = -0.9D1 / 0.8D1 * t53 * t22 * t55 * t60 - 0.18D2 
     #* t112 * wd * t114 * t119 - 0.9D1 / 0.8D1 * t122 * t22 * t55 * t60
     # - 0.18D2 * t127 * wd * t114 * t119 - 0.9D1 / 0.8D1 * t132 * t22 *
     # t55 * t60 - 0.18D2 * t137 * wd * t114 * t119 - 0.18D2 * t142 * wd
     # * t114 * t119 - 0.9D1 / 0.8D1 * t147 * t22 * t55 * t60

      end function



      doubleprecision function rrgg2gght16s1em1
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
      rrgg2gght16s1em1 = 0.0D0

      end function



      doubleprecision function rrgg2gght16s1em2
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
      rrgg2gght16s1em2 = 0.0D0

      end function



      doubleprecision function rrgg2gght16s1em3
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
      rrgg2gght16s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2gght16s1em4
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
      rrgg2gght16s1em4 = 0.0D0

      end function
