  
      subroutine rrgq2qght14
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgq2qght14s1e1  
      doubleprecision rrgq2qght14s1e0  
      doubleprecision rrgq2qght14s1em1  
      doubleprecision rrgq2qght14s1em2  
      doubleprecision rrgq2qght14s1em3  
      doubleprecision rrgq2qght14s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght14s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgq2qght14s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght14s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgq2qght14s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgq2qght14s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgq2qght14s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgq2qght14s1e1
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
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = -0.1D1 + z
      t4 = t3 * x1
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t9 = t1 ** 2
      t11 = t3 ** 2
      t15 = x1 * t6
      t16 = t11 * t3
      t17 = 0.1D1 / z
      t18 = t16 * t17
      t19 = t15 * t18
      t20 = x2 * pi
      t21 = cos(t20)
      t22 = t21 ** 2
      t23 = wd * t22
      t24 = sin(t20)
      t25 = t24 ** 2
      t26 = z ** 2
      t27 = 0.1D1 / t26
      t28 = t25 * t27
      t29 = t11 ** 2
      t30 = t28 * t29
      t31 = x1 ** 2
      t32 = t6 ** 2
      t33 = t31 * t32
      t34 = t9 ** 2
      t36 = t33 * x4 * t34
      t39 = log(0.4D1 * t30 * t36)
      t40 = t39 ** 2
      t43 = 0.1D1 / (-0.2D1 + t1)
      t48 = t15 * t16
      t49 = t17 * wd
      t54 = t15 * t18 * wd
      t55 = 0.90D2 * t54
      t59 = 0.180D3 * t48 * lh * t17 * wd
      t61 = (t55 - t59) * t22
      t62 = -0.90D2 * t48 * t49 * t22 + t61
      t64 = t34 * t43
      t66 = lh ** 2
      t68 = pi ** 2
      t70 = 0.180D3 * t66 - 0.30D2 * t68
      t76 = -t61 + (t55 - t59 + t48 * t70 * t17 * wd) * t22
      t78 = t76 * t34 * t43
      t80 = 0.1D1 / x4
      t83 = t16 * lh
      t84 = t15 * t83
      t89 = log(0.4D1 * t30 * t33 * t34)
      t90 = t89 * x1
      t91 = t6 * t16
      t96 = (-0.180D3 * t84 - 0.90D2 * t90 * t91) * t17 * wd
      t99 = t91 * lh
      t102 = t89 ** 2
      t103 = t102 * x1
      t108 = (t15 * t16 * t70 + 0.180D3 * t90 * t99 + 0.45D2 * t103 * t9
     #1) * t17 * wd
      t111 = t43 * t22
      t137 = x3 * t25
      t138 = t27 * t29
      t142 = log(0.4D1 * t137 * t138 * t36)
      t144 = t64 * t22
      t152 = 0.1D1 / x3
      t162 = log(0.4D1 * t137 * t27 * t29 * t31 * t32 * t34)
      t163 = t162 ** 2
      t174 = 0.4D1 / 0.45D2 * (-0.45D2 * t19 * t23 * t40 * t34 * t43 + t
     #62 * t39 * t64 - t78) * t80 + 0.4D1 / 0.45D2 * (t55 + t96 + t108) 
     #* t34 * t111 - 0.4D1 / 0.45D2 * (t55 + t96 + t108 + (t15 * t16 * (
     #0.60D2 * lh * t68 - 0.240D3 * zeta3 - 0.120D3 * t66 * lh) - t90 * 
     #t91 * t70 - 0.90D2 * t103 * t99 - 0.15D2 * t102 * t89 * x1 * t91) 
     #* t17 * wd) * t34 * t111 - (-0.360D3 * t19 * wd * t142 * t144 - 0.
     #720D3 * t84 * t49 * t144) * t152 * t80 / 0.45D2 + 0.4D1 / 0.45D2 *
     # (-0.45D2 * t19 * t23 * t163 * t34 * t43 + t62 * t162 * t64 - t78)
     # * t152
      t175 = FJET(XB1, XB2, s, t2 * t4, 0.0D0, 0.0D0, -t2 * t7, -s * t9 
     #* t11 * x1 * t6, t174)
      t177 = sqrt(x4)
      t178 = -0.1D1 + t177
      t179 = t177 + 0.1D1
      t180 = t178 * t179
      t181 = KAPPA2(x1, x2, 0.0D0, -t180, z)
      t182 = s * t181
      t184 = t7 * x4
      t188 = t6 * t178 * t179
      t190 = t181 ** 2
      t196 = t29 * t178
      t199 = t25 * x4
      t200 = t190 ** 2
      t205 = log(-0.4D1 * t33 * t196 * t179 * t27 * t199 * t200)
      t206 = t205 ** 2
      t208 = Sqrt(-t180)
      t209 = t208 ** 2
      t212 = 0.1D1 / (-0.2D1 + t181)
      t213 = t209 * t200 * t212
      t227 = x3 * x4
      t232 = log(-0.4D1 * t33 * t196 * t179 * t28 * t227 * t200)
      t235 = t22 * t200 * t212
      t240 = t15 * t83 * t17
      t249 = 0.4D1 / 0.45D2 * (0.45D2 * t54 * t22 * t206 * t213 - t62 * 
     #t205 * t213 + t76 * t209 * t200 * t212) * t80 - (0.360D3 * t54 * t
     #232 * t209 * t235 + 0.720D3 * t240 * wd * t209 * t235) * t152 * t8
     #0 / 0.45D2
      t250 = FJET(XB1, XB2, s, t182 * t4, 0.0D0, -t182 * t184, t182 * t3
     # * t188, s * t190 * t11 * t15 * (-0.1D1 + x4), t249)
      t252 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t253 = s * t252
      t255 = sqrt(x3)
      t256 = -0.1D1 + t255
      t258 = t255 + 0.1D1
      t259 = x1 * t256 * t258
      t261 = t4 * x3
      t264 = t252 ** 2
      t270 = t256 * t258
      t273 = t264 ** 2
      t279 = log(-0.4D1 * t270 * t33 * t29 * t28 * x4 * t273 * x3)
      t282 = 0.1D1 / (-0.2D1 + t252)
      t284 = Sqrt(-t270)
      t285 = t284 ** 2
      t286 = t282 * t22 * t285
      t304 = log(-0.4D1 * t270 * t33 * t138 * t25 * t273 * x3)
      t305 = t304 ** 2
      t308 = t273 * t282 * t285
      t320 = -(0.360D3 * t54 * t279 * t273 * t286 + 0.720D3 * t240 * wd 
     #* t273 * t286) * t152 * t80 / 0.45D2 + 0.4D1 / 0.45D2 * (0.45D2 * 
     #t54 * t22 * t305 * t308 - t62 * t304 * t308 + t76 * t273 * t282 * 
     #t285) * t152
      t321 = FJET(XB1, XB2, s, -t253 * t3 * t259, t253 * t261, 0.0D0, -t
     #253 * t7, s * t264 * t11 * t15 * (-0.1D1 + x3), t320)
      t323 = KAPPA2(x1, x2, x3, -t180, z)
      t324 = s * t323
      t325 = t324 * t3
      t330 = t323 ** 2
      t336 = Sqrt(t270 * t180)
      t347 = t330 ** 2
      t354 = log(0.4D1 * t270 * t31 * t32 * t29 * t178 * t179 * t347 * t
     #27 * t199 * x3)
      t363 = (-t255 * t177 + 0.2D1 * t21 * t336) ** 2
      t364 = t347 / (-0.2D1 + t323) * t363
      t371 = -0.90D2 * t19 * wd * t354 * t364 - 0.180D3 * t84 * t49 * t3
     #64
      t375 = FJET(XB1, XB2, s, -t325 * t259, t324 * t261, -t324 * t184, 
     #t325 * t188, s * t330 * t11 * t15 * (x3 - 0.1D1 + x4 - 0.2D1 * t22
     #7 + 0.2D1 * t21 * t255 * t177 * t336), -t371 * t152 * t80 / 0.45D2
     #)
      rrgq2qght14s1e1 = t175 * t174 + t250 * t249 + t321 * t320 - t375 *
     # t371 * t152 * t80 / 0.45D2

      end function



      doubleprecision function rrgq2qght14s1e0
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
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = -0.1D1 + z
      t4 = t3 * x1
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t9 = t1 ** 2
      t11 = t3 ** 2
      t15 = x1 * t6
      t16 = t11 * t3
      t17 = 0.1D1 / z
      t18 = t16 * t17
      t19 = t15 * t18
      t20 = x2 * pi
      t21 = cos(t20)
      t22 = t21 ** 2
      t23 = wd * t22
      t24 = sin(t20)
      t25 = t24 ** 2
      t26 = z ** 2
      t27 = 0.1D1 / t26
      t29 = t11 ** 2
      t30 = t25 * t27 * t29
      t31 = x1 ** 2
      t32 = t6 ** 2
      t33 = t31 * t32
      t34 = t9 ** 2
      t39 = log(0.4D1 * t30 * t33 * x4 * t34)
      t42 = 0.1D1 / (-0.2D1 + t1)
      t47 = t15 * t16
      t53 = t15 * t18 * wd
      t54 = 0.90D2 * t53
      t61 = -0.90D2 * t47 * t17 * wd * t22 + (t54 - 0.180D3 * t47 * lh *
     # t17 * wd) * t22
      t63 = t61 * t34 * t42
      t65 = 0.1D1 / x4
      t69 = 0.1D1 / x3
      t82 = log(0.4D1 * x3 * t25 * t27 * t29 * t31 * t32 * t34)
      t97 = log(0.4D1 * t30 * t33 * t34)
      t98 = t97 * x1
      t99 = t6 * t16
      t104 = (-0.180D3 * t15 * t16 * lh - 0.90D2 * t98 * t99) * t17 * wd
      t107 = t42 * t22
      t110 = lh ** 2
      t112 = pi ** 2
      t120 = t97 ** 2
      t131 = 0.4D1 / 0.45D2 * (0.90D2 * t19 * t23 * t39 * t34 * t42 - t6
     #3) * t65 - 0.8D1 * t53 * t34 * t42 * t22 * t69 * t65 + 0.4D1 / 0.4
     #5D2 * (0.90D2 * t19 * t23 * t82 * t34 * t42 - t63) * t69 + 0.4D1 /
     # 0.45D2 * (t54 + t104) * t34 * t107 - 0.4D1 / 0.45D2 * (t54 + t104
     # + (t15 * t16 * (0.180D3 * t110 - 0.30D2 * t112) + 0.180D3 * t98 *
     # t99 * lh + 0.45D2 * t120 * x1 * t99) * t17 * wd) * t34 * t107
      t132 = FJET(XB1, XB2, s, t2 * t4, 0.0D0, 0.0D0, -t2 * t7, -s * t9 
     #* t11 * x1 * t6, t131)
      t134 = sqrt(x4)
      t135 = -0.1D1 + t134
      t136 = t134 + 0.1D1
      t137 = t135 * t136
      t138 = KAPPA2(x1, x2, 0.0D0, -t137, z)
      t139 = s * t138
      t141 = t7 * x4
      t145 = t6 * t135 * t136
      t147 = t138 ** 2
      t157 = t147 ** 2
      t162 = log(-0.4D1 * t33 * t29 * t135 * t136 * t27 * t25 * x4 * t15
     #7)
      t164 = Sqrt(-t137)
      t165 = t164 ** 2
      t168 = 0.1D1 / (-0.2D1 + t138)
      t186 = 0.4D1 / 0.45D2 * (-0.90D2 * t53 * t22 * t162 * t165 * t157 
     #* t168 + t61 * t165 * t157 * t168) * t65 + 0.8D1 * t53 * t165 * t2
     #2 * t157 * t168 * t69 * t65
      t187 = FJET(XB1, XB2, s, t139 * t4, 0.0D0, -t139 * t141, t139 * t3
     # * t145, s * t147 * t11 * t15 * (-0.1D1 + x4), t186)
      t189 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t190 = s * t189
      t192 = sqrt(x3)
      t193 = -0.1D1 + t192
      t195 = t192 + 0.1D1
      t196 = x1 * t193 * t195
      t198 = t4 * x3
      t201 = t189 ** 2
      t207 = t201 ** 2
      t209 = 0.1D1 / (-0.2D1 + t189)
      t210 = t207 * t209
      t212 = t193 * t195
      t213 = Sqrt(-t212)
      t214 = t213 ** 2
      t227 = log(-0.4D1 * t212 * t33 * t29 * t27 * t25 * t207 * x3)
      t239 = 0.8D1 * t53 * t210 * t22 * t214 * t69 * t65 + 0.4D1 / 0.45D
     #2 * (-0.90D2 * t53 * t22 * t227 * t210 * t214 + t61 * t207 * t209 
     #* t214) * t69
      t240 = FJET(XB1, XB2, s, -t190 * t3 * t196, t190 * t198, 0.0D0, -t
     #190 * t7, s * t201 * t11 * t15 * (-0.1D1 + x3), t239)
      t242 = KAPPA2(x1, x2, x3, -t137, z)
      t243 = s * t242
      t244 = t243 * t3
      t249 = t242 ** 2
      t256 = Sqrt(t212 * t137)
      t263 = t249 ** 2
      t265 = 0.1D1 / (-0.2D1 + t242)
      t271 = (-t192 * t134 + 0.2D1 * t21 * t256) ** 2
      t273 = t271 * t69 * t65
      t277 = FJET(XB1, XB2, s, -t244 * t196, t243 * t198, -t243 * t141, 
     #t244 * t145, s * t249 * t11 * t15 * (x3 - 0.1D1 + x4 - 0.2D1 * x3 
     #* x4 + 0.2D1 * t21 * t192 * t134 * t256), -0.2D1 * t53 * t263 * t2
     #65 * t273)
      rrgq2qght14s1e0 = t132 * t131 + t187 * t186 + t240 * t239 - 0.2D1 
     #* t277 * x1 * t99 * t17 * wd * t263 * t265 * t273

      end function



      doubleprecision function rrgq2qght14s1em1
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
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = -0.1D1 + z
      t4 = t3 * x1
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t9 = t1 ** 2
      t11 = t3 ** 2
      t15 = x1 * t6
      t16 = t11 * t3
      t17 = 0.1D1 / z
      t18 = t16 * t17
      t19 = t15 * t18
      t20 = t9 ** 2
      t21 = wd * t20
      t24 = x2 * pi
      t25 = cos(t24)
      t26 = t25 ** 2
      t27 = 0.1D1 / (-0.2D1 + t1) * t26
      t28 = 0.1D1 / x3
      t37 = t15 * t18 * wd
      t42 = sin(t24)
      t43 = t42 ** 2
      t44 = z ** 2
      t47 = t11 ** 2
      t49 = x1 ** 2
      t50 = t6 ** 2
      t55 = log(0.4D1 * t43 / t44 * t47 * t49 * t50 * t20)
      t57 = t6 * t16
      t67 = 0.1D1 / x4
      t72 = -0.8D1 * t19 * t21 * t27 * t28 + 0.8D1 * t19 * t21 * t27 - 0
     #.4D1 / 0.45D2 * (0.90D2 * t37 + (-0.180D3 * t15 * t16 * lh - 0.90D
     #2 * t55 * x1 * t57) * t17 * wd) * t20 * t27 - 0.8D1 * t19 * t21 * 
     #t27 * t67
      t73 = FJET(XB1, XB2, s, t2 * t4, 0.0D0, 0.0D0, -t2 * t7, -s * t9 *
     # t11 * x1 * t6, t72)
      t75 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t76 = s * t75
      t78 = sqrt(x3)
      t79 = -0.1D1 + t78
      t81 = t78 + 0.1D1
      t87 = t75 ** 2
      t93 = t87 ** 2
      t98 = Sqrt(-t79 * t81)
      t99 = t98 ** 2
      t101 = 0.1D1 / (-0.2D1 + t75) * t99 * t28
      t105 = FJET(XB1, XB2, s, -t76 * t3 * x1 * t79 * t81, t76 * t4 * x3
     #, 0.0D0, -t76 * t7, s * t87 * t11 * t15 * (-0.1D1 + x3), 0.8D1 * t
     #37 * t26 * t93 * t101)
      t107 = t57 * t17
      t109 = wd * t26
      t114 = sqrt(x4)
      t115 = -0.1D1 + t114
      t116 = t114 + 0.1D1
      t117 = t115 * t116
      t118 = KAPPA2(x1, x2, 0.0D0, -t117, z)
      t119 = s * t118
      t127 = t118 ** 2
      t133 = Sqrt(-t117)
      t134 = t133 ** 2
      t136 = t127 ** 2
      t140 = t136 / (-0.2D1 + t118) * t67
      t144 = FJET(XB1, XB2, s, t119 * t4, 0.0D0, -t119 * t7 * x4, t119 *
     # t3 * t6 * t115 * t116, s * t127 * t11 * t15 * (-0.1D1 + x4), 0.8D
     #1 * t37 * t26 * t134 * t140)
      rrgq2qght14s1em1 = t73 * t72 + 0.8D1 * t105 * x1 * t107 * t109 * t
     #93 * t101 + 0.8D1 * t144 * x1 * t107 * t109 * t134 * t140

      end function



      doubleprecision function rrgq2qght14s1em2
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
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = -0.1D1 + z
      t6 = -0.1D1 + x1
      t9 = t1 ** 2
      t11 = t3 ** 2
      t16 = t11 * t3
      t17 = 0.1D1 / z
      t20 = t9 ** 2
      t23 = 0.1D1 / (-0.2D1 + t1)
      t25 = cos(x2 * pi)
      t26 = t25 ** 2
      t31 = FJET(XB1, XB2, s, t2 * t3 * x1, 0.0D0, 0.0D0, -t2 * t3 * t6,
     # -s * t9 * t11 * x1 * t6, -0.8D1 * x1 * t6 * t16 * t17 * wd * t20 
     #* t23 * t26)
      rrgq2qght14s1em2 = -0.8D1 * t31 * x1 * t6 * t16 * t17 * wd * t20 *
     # t23 * t26

      end function



      doubleprecision function rrgq2qght14s1em3
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
      rrgq2qght14s1em3 = 0.0D0

      end function



      doubleprecision function rrgq2qght14s1em4
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
      rrgq2qght14s1em4 = 0.0D0

      end function
