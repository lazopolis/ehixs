      subroutine rrgg2gght15
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      if(z.eq.1d0)then
         call rrgg2gghsoftt15
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      else
         call rrgg2gghhardt15
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      end if
      end subroutine

  
      subroutine rrgg2gghhardt15
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2gghhardt15s1e1  
      doubleprecision rrgg2gghhardt15s1e0  
      doubleprecision rrgg2gghhardt15s1em1  
      doubleprecision rrgg2gghhardt15s1em2  
      doubleprecision rrgg2gghhardt15s1em3  
      doubleprecision rrgg2gghhardt15s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt15s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt15s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt15s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt15s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt15s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt15s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gghhardt15s1e1
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
      t5 = t2 * t4
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t8 = t2 * t7
      t9 = t1 ** 2
      t11 = t3 ** 2
      t14 = s * t9 * t11 * x1 * t6
      t15 = 0.1D1 / z
      t16 = t6 * t15
      t17 = t11 ** 2
      t18 = x1 * t17
      t19 = t16 * t18
      t20 = x2 * pi
      t21 = cos(t20)
      t22 = t21 ** 2
      t23 = wd * t22
      t24 = sin(t20)
      t25 = t24 ** 2
      t26 = z ** 2
      t27 = 0.1D1 / t26
      t28 = t25 * t27
      t29 = x1 ** 2
      t31 = t6 ** 2
      t33 = t9 ** 2
      t37 = log(0.4D1 * t28 * t29 * t31 * x4 * t33)
      t38 = t37 ** 2
      t41 = 0.1D1 / (-0.2D1 + t1)
      t47 = t16 * t18 * wd
      t50 = t16 * x1 * lh
      t51 = t16 * x1
      t55 = (-0.180D3 * t50 + 0.180D3 * t51) * t17 * wd
      t56 = -0.180D3 * t47 + t55
      t57 = t56 * t22
      t63 = pi ** 2
      t65 = lh ** 2
      t67 = -0.30D2 * t63 + 0.180D3 * t65
      t69 = t16 * x1 * t67
      t76 = (0.90D2 * t47 - 0.2D1 * t55 + (t69 - 0.360D3 * t50 + 0.270D3
     # * t51) * t17 * wd) * t22
      t77 = t33 * t41
      t78 = t76 * t77
      t80 = 0.1D1 / x4
      t85 = t29 * t31
      t86 = t85 * t33
      t89 = log(0.4D1 * t28 * t86)
      t91 = t89 * t6 * t15
      t93 = (0.2D1 * t16 - t91) * x1
      t97 = t77 * wd
      t103 = t89 ** 2
      t105 = t103 * t6 * t15
      t108 = (0.3D1 * t16 - 0.2D1 * t91 + t105 / 0.2D1) * x1
      t141 = x3 * t25 * t27
      t146 = log(0.4D1 * t141 * t85 * x4 * t33)
      t157 = 0.1D1 / x3
      t163 = log(0.4D1 * t141 * t86)
      t164 = t163 ** 2
      t176 = -(-0.45D2 * t19 * t23 * t38 * t33 * t41 + t57 * t37 * t33 *
     # t41 - t78) * t80 / 0.10D2 + ((-0.180D3 * t50 + 0.90D2 * t93) * t1
     #7 * t97 - 0.2D1 * (t69 - 0.180D3 * t93 * lh + 0.90D2 * t108) * t17
     # * t97 + (t16 * x1 * (-0.240D3 * zeta3 - 0.120D3 * t65 * lh + 0.60
     #D2 * lh * t63) + t93 * t67 - 0.180D3 * t108 * lh + 0.90D2 * (0.4D1
     # * t16 - 0.3D1 * t91 + t105 - t103 * t89 * t6 * t15 / 0.6D1) * x1)
     # * t17 * t97) * t22 / 0.10D2 - (0.360D3 * t19 * wd * t146 * t77 * 
     #t22 - 0.4D1 * t56 * t33 * t41 * t22) * t157 * t80 / 0.40D2 - (-0.4
     #5D2 * t19 * t23 * t164 * t33 * t41 + t57 * t163 * t33 * t41 - t78)
     # * t157 / 0.10D2
      t177 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t176)
      t179 = sqrt(x4)
      t180 = -0.1D1 + t179
      t181 = t179 + 0.1D1
      t182 = t180 * t181
      t183 = KAPPA2(x1, x2, 0.0D0, -t182, z)
      t184 = s * t183
      t185 = t184 * t4
      t188 = t6 * t180 * t181
      t189 = t184 * t3 * t188
      t190 = t7 * x4
      t191 = t184 * t190
      t192 = t183 ** 2
      t195 = x1 * t6
      t198 = s * t192 * t11 * t195 * (-0.1D1 + x4)
      t199 = t31 * t27
      t200 = t199 * t182
      t201 = t25 * t29
      t202 = t192 ** 2
      t203 = x4 * t202
      t207 = log(-0.4D1 * t200 * t201 * t203)
      t208 = t207 ** 2
      t210 = Sqrt(-t182)
      t211 = t210 ** 2
      t214 = 0.1D1 / (-0.2D1 + t183)
      t215 = t211 * t202 * t214
      t229 = log(-0.4D1 * t200 * t201 * t203 * x3)
      t232 = t202 * t214 * t22
      t243 = -(0.45D2 * t47 * t22 * t208 * t215 - t57 * t207 * t215 + t7
     #6 * t215) * t80 / 0.10D2 - (-0.360D3 * t47 * t229 * t211 * t232 + 
     #0.4D1 * t56 * t211 * t232) * t157 * t80 / 0.40D2
      t244 = FJET(XB1, XB2, s, 0.0D0, t185, t189, -t191, t198, t243)
      t246 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t176)
      t248 = FJET(XB1, XB2, s, t185, 0.0D0, -t191, t189, t198, t243)
      t250 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t251 = s * t250
      t252 = t4 * x3
      t253 = t251 * t252
      t255 = sqrt(x3)
      t256 = -0.1D1 + t255
      t258 = t255 + 0.1D1
      t259 = x1 * t256 * t258
      t260 = t251 * t3 * t259
      t261 = t251 * t7
      t262 = t250 ** 2
      t267 = s * t262 * t11 * t195 * (-0.1D1 + x3)
      t268 = t256 * t258
      t269 = t199 * t268
      t270 = t262 ** 2
      t276 = log(-0.4D1 * t269 * t201 * t270 * x4 * x3)
      t279 = 0.1D1 / (-0.2D1 + t250)
      t281 = Sqrt(-t268)
      t282 = t281 ** 2
      t283 = t279 * t22 * t282
      t298 = log(-0.4D1 * t269 * t201 * t270 * x3)
      t299 = t298 ** 2
      t302 = t270 * t279 * t282
      t312 = -(-0.360D3 * t47 * t276 * t270 * t283 + 0.4D1 * t56 * t270 
     #* t283) * t157 * t80 / 0.40D2 - (0.45D2 * t47 * t22 * t299 * t302 
     #- t57 * t298 * t302 + t76 * t302) * t157 / 0.10D2
      t313 = FJET(XB1, XB2, s, t253, -t260, -t261, 0.0D0, t267, t312)
      t315 = KAPPA2(x1, x2, x3, -t182, z)
      t316 = s * t315
      t317 = t316 * t252
      t318 = t316 * t3
      t319 = t318 * t259
      t320 = t318 * t188
      t321 = t316 * t190
      t322 = t315 ** 2
      t329 = Sqrt(t268 * t182)
      t335 = s * t322 * t11 * t195 * (x3 - 0.1D1 + x4 - 0.2D1 * x3 * x4 
     #+ 0.2D1 * t21 * t255 * t179 * t329)
      t336 = t322 ** 2
      t347 = log(0.4D1 * t31 * t336 * t27 * t180 * t181 * t268 * t25 * t
     #29 * x4 * x3)
      t353 = (t255 * t179 - 0.2D1 * t21 * t329) ** 2
      t356 = 0.1D1 / (-0.2D1 + t315)
      t364 = 0.90D2 * t19 * wd * t347 * t336 * t353 * t356 - t56 * t336 
     #* t353 * t356
      t367 = t364 * t157 * t80 / 0.40D2
      t368 = FJET(XB1, XB2, s, t317, -t319, t320, -t321, t335, -t367)
      t370 = t157 * t80
      t373 = FJET(XB1, XB2, s, -t260, t253, 0.0D0, -t261, t267, t312)
      t375 = FJET(XB1, XB2, s, -t319, t317, -t321, t320, t335, -t367)
      rrgg2gghhardt15s1e1 = t177 * t176 + t244 * t243 + t246 * t176 + t2
     #48 * t243 + t313 * t312 - t368 * t364 * t370 / 0.40D2 + t373 * t31
     #2 - t375 * t364 * t370 / 0.40D2

      end function



      doubleprecision function rrgg2gghhardt15s1e0
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
      t5 = t2 * t4
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t8 = t2 * t7
      t9 = t1 ** 2
      t11 = t3 ** 2
      t14 = s * t9 * t11 * x1 * t6
      t15 = 0.1D1 / z
      t16 = t6 * t15
      t17 = t11 ** 2
      t18 = x1 * t17
      t19 = t16 * t18
      t20 = x2 * pi
      t21 = cos(t20)
      t22 = t21 ** 2
      t23 = wd * t22
      t24 = sin(t20)
      t25 = t24 ** 2
      t26 = z ** 2
      t27 = 0.1D1 / t26
      t28 = t25 * t27
      t29 = x1 ** 2
      t31 = t6 ** 2
      t33 = t9 ** 2
      t37 = log(0.4D1 * t28 * t29 * t31 * x4 * t33)
      t40 = 0.1D1 / (-0.2D1 + t1)
      t46 = t16 * t18 * wd
      t49 = t16 * x1 * lh
      t50 = t16 * x1
      t55 = -0.180D3 * t46 + (-0.180D3 * t49 + 0.180D3 * t50) * t17 * wd
      t58 = t55 * t33 * t40 * t22
      t60 = 0.1D1 / x4
      t63 = t33 * t40
      t64 = 0.1D1 / x3
      t66 = t22 * t64 * t60
      t73 = t29 * t31 * t33
      t76 = log(0.4D1 * x3 * t25 * t27 * t73)
      t94 = log(0.4D1 * t28 * t73)
      t96 = t94 * t6 * t15
      t98 = (0.2D1 * t16 - t96) * x1
      t102 = t63 * wd
      t105 = pi ** 2
      t107 = lh ** 2
      t116 = t94 ** 2
      t129 = -(0.90D2 * t19 * t23 * t37 * t33 * t40 - t58) * t60 / 0.10D
     #2 + 0.9D1 * t46 * t63 * t66 - (0.90D2 * t19 * t23 * t76 * t33 * t4
     #0 - t58) * t64 / 0.10D2 + (0.90D2 * t50 * t17 * t33 * t40 * wd - 0
     #.2D1 * (-0.180D3 * t49 + 0.90D2 * t98) * t17 * t102 + (t16 * x1 * 
     #(-0.30D2 * t105 + 0.180D3 * t107) - 0.180D3 * t98 * lh + 0.90D2 * 
     #(0.3D1 * t16 - 0.2D1 * t96 + t116 * t6 * t15 / 0.2D1) * x1) * t17 
     #* t102) * t22 / 0.10D2
      t130 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t129)
      t132 = sqrt(x4)
      t133 = -0.1D1 + t132
      t134 = t132 + 0.1D1
      t135 = t133 * t134
      t136 = KAPPA2(x1, x2, 0.0D0, -t135, z)
      t137 = s * t136
      t138 = t137 * t4
      t141 = t6 * t133 * t134
      t142 = t137 * t3 * t141
      t143 = t7 * x4
      t144 = t137 * t143
      t145 = t136 ** 2
      t148 = x1 * t6
      t151 = s * t145 * t11 * t148 * (-0.1D1 + x4)
      t152 = t31 * t27
      t154 = t25 * t29
      t155 = t145 ** 2
      t160 = log(-0.4D1 * t152 * t135 * t154 * x4 * t155)
      t162 = Sqrt(-t135)
      t163 = t162 ** 2
      t166 = 0.1D1 / (-0.2D1 + t136)
      t167 = t163 * t155 * t166
      t181 = -(-0.90D2 * t46 * t22 * t160 * t167 + t55 * t163 * t155 * t
     #166 * t22) * t60 / 0.10D2 - 0.9D1 * t46 * t167 * t66
      t182 = FJET(XB1, XB2, s, 0.0D0, t138, t142, -t144, t151, t181)
      t184 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t129)
      t186 = FJET(XB1, XB2, s, t138, 0.0D0, -t144, t142, t151, t181)
      t188 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t189 = s * t188
      t190 = t4 * x3
      t191 = t189 * t190
      t193 = sqrt(x3)
      t194 = -0.1D1 + t193
      t196 = t193 + 0.1D1
      t197 = x1 * t194 * t196
      t198 = t189 * t3 * t197
      t199 = t189 * t7
      t200 = t188 ** 2
      t205 = s * t200 * t11 * t148 * (-0.1D1 + x3)
      t206 = t200 ** 2
      t208 = 0.1D1 / (-0.2D1 + t188)
      t209 = t206 * t208
      t211 = t194 * t196
      t212 = Sqrt(-t211)
      t213 = t212 ** 2
      t224 = log(-0.4D1 * t152 * t211 * t154 * t206 * x3)
      t237 = -0.9D1 * t46 * t209 * t22 * t213 * t64 * t60 - (-0.90D2 * t
     #46 * t22 * t224 * t209 * t213 + t55 * t206 * t208 * t22 * t213) * 
     #t64 / 0.10D2
      t238 = FJET(XB1, XB2, s, t191, -t198, -t199, 0.0D0, t205, t237)
      t240 = KAPPA2(x1, x2, x3, -t135, z)
      t241 = s * t240
      t242 = t241 * t190
      t243 = t241 * t3
      t244 = t243 * t197
      t245 = t243 * t141
      t246 = t241 * t143
      t247 = t240 ** 2
      t254 = Sqrt(t211 * t135)
      t260 = s * t247 * t11 * t148 * (x3 - 0.1D1 + x4 - 0.2D1 * x3 * x4 
     #+ 0.2D1 * t21 * t193 * t132 * t254)
      t261 = t247 ** 2
      t266 = (t193 * t132 - 0.2D1 * t21 * t254) ** 2
      t271 = 0.1D1 / (-0.2D1 + t240) * t64 * t60
      t274 = 0.9D1 / 0.4D1 * t46 * t261 * t266 * t271
      t275 = FJET(XB1, XB2, s, t242, -t244, t245, -t246, t260, t274)
      t278 = t15 * x1 * t17
      t282 = wd * t261 * t266 * t271
      t285 = FJET(XB1, XB2, s, -t198, t191, 0.0D0, -t199, t205, t237)
      t287 = FJET(XB1, XB2, s, -t244, t242, -t246, t245, t260, t274)
      rrgg2gghhardt15s1e0 = t130 * t129 + t182 * t181 + t184 * t129 + t1
     #86 * t181 + t238 * t237 + 0.9D1 / 0.4D1 * t275 * t6 * t278 * t282 
     #+ t285 * t237 + 0.9D1 / 0.4D1 * t287 * t6 * t278 * t282

      end function



      doubleprecision function rrgg2gghhardt15s1em1
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
      t5 = t2 * t4
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t8 = t2 * t7
      t9 = t1 ** 2
      t11 = t3 ** 2
      t14 = s * t9 * t11 * x1 * t6
      t15 = 0.1D1 / z
      t16 = t6 * t15
      t17 = t11 ** 2
      t18 = x1 * t17
      t19 = t16 * t18
      t20 = t9 ** 2
      t21 = wd * t20
      t23 = 0.1D1 / (-0.2D1 + t1)
      t24 = x2 * pi
      t25 = cos(t24)
      t26 = t25 ** 2
      t27 = t23 * t26
      t28 = 0.1D1 / x3
      t43 = sin(t24)
      t44 = t43 ** 2
      t45 = z ** 2
      t48 = x1 ** 2
      t49 = t6 ** 2
      t54 = log(0.4D1 * t44 / t45 * t48 * t49 * t20)
      t68 = 0.1D1 / x4
      t73 = 0.9D1 * t19 * t21 * t27 * t28 + (-0.180D3 * t16 * x1 * t17 *
     # t20 * t23 * wd + (-0.180D3 * t16 * x1 * lh + 0.90D2 * (0.2D1 * t1
     #6 - t54 * t6 * t15) * x1) * t17 * t20 * t23 * wd) * t26 / 0.10D2 +
     # 0.9D1 * t19 * t21 * t27 * t68
      t74 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t73)
      t76 = sqrt(x4)
      t77 = -0.1D1 + t76
      t78 = t76 + 0.1D1
      t79 = t77 * t78
      t80 = KAPPA2(x1, x2, 0.0D0, -t79, z)
      t81 = s * t80
      t82 = t81 * t4
      t86 = t81 * t3 * t6 * t77 * t78
      t88 = t81 * t7 * x4
      t89 = t80 ** 2
      t92 = x1 * t6
      t95 = s * t89 * t11 * t92 * (-0.1D1 + x4)
      t97 = t16 * t18 * wd
      t98 = Sqrt(-t79)
      t99 = t98 ** 2
      t100 = t89 ** 2
      t105 = 0.1D1 / (-0.2D1 + t80) * t26 * t68
      t108 = 0.9D1 * t97 * t99 * t100 * t105
      t109 = FJET(XB1, XB2, s, 0.0D0, t82, t86, -t88, t95, -t108)
      t112 = t15 * x1 * t17
      t116 = wd * t99 * t100 * t105
      t119 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t73)
      t121 = FJET(XB1, XB2, s, t82, 0.0D0, -t88, t86, t95, -t108)
      t126 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t127 = s * t126
      t129 = t127 * t4 * x3
      t131 = sqrt(x3)
      t132 = -0.1D1 + t131
      t134 = t131 + 0.1D1
      t136 = t127 * t3 * x1 * t132 * t134
      t137 = t127 * t7
      t138 = t126 ** 2
      t143 = s * t138 * t11 * t92 * (-0.1D1 + x3)
      t144 = t138 ** 2
      t146 = 0.1D1 / (-0.2D1 + t126)
      t149 = Sqrt(-t132 * t134)
      t150 = t149 ** 2
      t152 = t26 * t150 * t28
      t155 = 0.9D1 * t97 * t144 * t146 * t152
      t156 = FJET(XB1, XB2, s, t129, -t136, -t137, 0.0D0, t143, -t155)
      t161 = wd * t144 * t146 * t152
      t164 = FJET(XB1, XB2, s, -t136, t129, 0.0D0, -t137, t143, -t155)
      rrgg2gghhardt15s1em1 = t74 * t73 - 0.9D1 * t109 * t6 * t112 * t116
     # + t119 * t73 - 0.9D1 * t121 * t6 * t112 * t116 - 0.9D1 * t156 * t
     #6 * t112 * t161 - 0.9D1 * t164 * t6 * t112 * t161

      end function



      doubleprecision function rrgg2gghhardt15s1em2
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
      t5 = t2 * t3 * x1
      t6 = -0.1D1 + x1
      t8 = t2 * t3 * t6
      t9 = t1 ** 2
      t11 = t3 ** 2
      t14 = s * t9 * t11 * x1 * t6
      t15 = 0.1D1 / z
      t17 = t11 ** 2
      t20 = t9 ** 2
      t23 = 0.1D1 / (-0.2D1 + t1)
      t25 = cos(x2 * pi)
      t26 = t25 ** 2
      t30 = 0.9D1 * t6 * t15 * x1 * t17 * wd * t20 * t23 * t26
      t31 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t30)
      t33 = t15 * x1
      t38 = t17 * wd * t20 * t23 * t26
      t40 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t30)
      rrgg2gghhardt15s1em2 = 0.9D1 * t31 * t6 * t33 * t38 + 0.9D1 * t40 
     #* t6 * t33 * t38

      end function



      doubleprecision function rrgg2gghhardt15s1em3
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
      rrgg2gghhardt15s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2gghhardt15s1em4
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
      rrgg2gghhardt15s1em4 = 0.0D0

      end function
  
      subroutine rrgg2gghsoftt15
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2gghsoftt15s1e1  
      doubleprecision rrgg2gghsoftt15s1e0  
      doubleprecision rrgg2gghsoftt15s1em1  
      doubleprecision rrgg2gghsoftt15s1em2  
      doubleprecision rrgg2gghsoftt15s1em3  
      doubleprecision rrgg2gghsoftt15s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt15s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt15s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt15s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt15s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt15s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt15s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gghsoftt15s1e1
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
      rrgg2gghsoftt15s1e1 = 0.0D0

      end function



      doubleprecision function rrgg2gghsoftt15s1e0
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
      rrgg2gghsoftt15s1e0 = 0.0D0

      end function



      doubleprecision function rrgg2gghsoftt15s1em1
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
      rrgg2gghsoftt15s1em1 = 0.0D0

      end function



      doubleprecision function rrgg2gghsoftt15s1em2
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
      rrgg2gghsoftt15s1em2 = 0.0D0

      end function



      doubleprecision function rrgg2gghsoftt15s1em3
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
      rrgg2gghsoftt15s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2gghsoftt15s1em4
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
      rrgg2gghsoftt15s1em4 = 0.0D0

      end function
