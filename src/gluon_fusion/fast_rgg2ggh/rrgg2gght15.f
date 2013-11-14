  
      subroutine rrgg2gght15
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2gght15s1e1  
      doubleprecision rrgg2gght15s1e0  
      doubleprecision rrgg2gght15s1em1  
      doubleprecision rrgg2gght15s1em2  
      doubleprecision rrgg2gght15s1em3  
      doubleprecision rrgg2gght15s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght15s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gght15s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght15s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gght15s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gght15s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gght15s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gght15s1e1
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
      t15 = t11 ** 2
      t16 = wd * t15
      t17 = x1 * t6
      t18 = t16 * t17
      t19 = 0.1D1 / z
      t20 = x2 * pi
      t21 = cos(t20)
      t22 = t21 ** 2
      t23 = t19 * t22
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
      t49 = t16 * x1 * t6 * t19 * t22
      t52 = t16 * t17 * lh
      t55 = (-0.180D3 * t52 + 0.180D3 * t18) * t19
      t56 = t55 * t22
      t57 = -0.180D3 * t49 + t56
      t59 = t33 * t41
      t63 = pi ** 2
      t65 = lh ** 2
      t67 = -0.30D2 * t63 + 0.180D3 * t65
      t69 = t16 * t17 * t67
      t75 = 0.90D2 * t49 - 0.2D1 * t56 + (t69 - 0.360D3 * t52 + 0.270D3 
     #* t18) * t19 * t22
      t77 = t75 * t33 * t41
      t79 = 0.1D1 / x4
      t84 = t29 * t31
      t85 = t84 * t33
      t88 = log(0.4D1 * t28 * t85)
      t90 = t88 * wd * t15
      t92 = (0.2D1 * t16 - t90) * x1
      t97 = t59 * t22
      t100 = t6 * lh
      t105 = t88 ** 2
      t107 = t105 * wd * t15
      t110 = (0.3D1 * t16 - 0.2D1 * t90 + t107 / 0.2D1) * x1
      t144 = x3 * t25 * t27
      t149 = log(0.4D1 * t144 * t84 * x4 * t33)
      t155 = t16 * t17 * t19
      t157 = -0.180D3 * t155 + t55
      t163 = 0.1D1 / x3
      t169 = log(0.4D1 * t144 * t85)
      t170 = t169 ** 2
      t181 = -(-0.45D2 * t18 * t23 * t38 * t33 * t41 + t57 * t37 * t59 -
     # t77) * t79 / 0.10D2 + (-0.180D3 * t52 + 0.90D2 * t92 * t6) * t19 
     #* t97 / 0.10D2 - (t69 - 0.180D3 * t92 * t100 + 0.90D2 * t110 * t6)
     # * t19 * t97 / 0.5D1 + (t16 * t17 * (-0.240D3 * zeta3 - 0.120D3 * 
     #t65 * lh + 0.60D2 * lh * t63) + t92 * t6 * t67 - 0.180D3 * t110 * 
     #t100 + 0.90D2 * (0.4D1 * t16 - 0.3D1 * t90 + t107 - t105 * t88 * w
     #d * t15 / 0.6D1) * x1 * t6) * t19 * t97 / 0.10D2 + (-0.360D3 * t18
     # * t19 * t149 * t97 + 0.4D1 * t157 * t33 * t41 * t22) * t163 * t79
     # / 0.40D2 - (-0.45D2 * t18 * t23 * t170 * t33 * t41 + t57 * t169 *
     # t59 - t77) * t163 / 0.10D2
      t182 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t181)
      t184 = sqrt(x4)
      t185 = -0.1D1 + t184
      t186 = t184 + 0.1D1
      t187 = t185 * t186
      t188 = KAPPA2(x1, x2, 0.0D0, -t187, z)
      t189 = s * t188
      t190 = t189 * t4
      t193 = t6 * t185 * t186
      t194 = t189 * t3 * t193
      t195 = t7 * x4
      t196 = t189 * t195
      t197 = t188 ** 2
      t202 = s * t197 * t11 * t17 * (-0.1D1 + x4)
      t203 = x4 * t29
      t204 = t31 * t25
      t205 = t203 * t204
      t206 = t197 ** 2
      t211 = log(-0.4D1 * t205 * t187 * t27 * t206)
      t212 = t211 ** 2
      t214 = Sqrt(-t187)
      t215 = t214 ** 2
      t217 = 0.1D1 / (-0.2D1 + t188)
      t219 = t215 * t217 * t206
      t231 = t27 * x3
      t236 = log(-0.4D1 * t205 * t187 * t231 * t206)
      t239 = t217 * t22 * t206
      t250 = -(0.45D2 * t155 * t22 * t212 * t219 - t57 * t211 * t219 + t
     #75 * t215 * t217 * t206) * t79 / 0.10D2 + (0.360D3 * t155 * t236 *
     # t215 * t239 - 0.4D1 * t157 * t215 * t239) * t163 * t79 / 0.40D2
      t251 = FJET(XB1, XB2, s, 0.0D0, t190, t194, -t196, t202, t250)
      t253 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t181)
      t255 = FJET(XB1, XB2, s, t190, 0.0D0, -t196, t194, t202, t250)
      t257 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t258 = s * t257
      t259 = t4 * x3
      t260 = t258 * t259
      t262 = sqrt(x3)
      t263 = -0.1D1 + t262
      t265 = t262 + 0.1D1
      t266 = x1 * t263 * t265
      t267 = t258 * t3 * t266
      t268 = t258 * t7
      t269 = t257 ** 2
      t274 = s * t269 * t11 * t17 * (-0.1D1 + x3)
      t275 = t269 ** 2
      t280 = t263 * t27 * x3
      t284 = log(-0.4D1 * x4 * t275 * t84 * t25 * t265 * t280)
      t287 = 0.1D1 / (-0.2D1 + t257)
      t289 = t263 * t265
      t290 = Sqrt(-t289)
      t291 = t290 ** 2
      t292 = t287 * t22 * t291
      t308 = log(-0.4D1 * t275 * t29 * t204 * t289 * t231)
      t309 = t308 ** 2
      t312 = t275 * t287 * t291
      t324 = (0.360D3 * t155 * t284 * t275 * t292 - 0.4D1 * t157 * t275 
     #* t292) * t163 * t79 / 0.40D2 - (0.45D2 * t155 * t22 * t309 * t312
     # - t57 * t308 * t312 + t75 * t275 * t287 * t291) * t163 / 0.10D2
      t325 = FJET(XB1, XB2, s, t260, -t267, -t268, 0.0D0, t274, t324)
      t327 = KAPPA2(x1, x2, x3, -t187, z)
      t328 = s * t327
      t329 = t328 * t259
      t330 = t328 * t3
      t331 = t330 * t266
      t332 = t330 * t193
      t333 = t328 * t195
      t334 = t327 ** 2
      t341 = Sqrt(t289 * t187)
      t347 = s * t334 * t11 * t17 * (x3 - 0.1D1 + x4 - 0.2D1 * x3 * x4 +
     # 0.2D1 * t21 * t262 * t184 * t341)
      t350 = t334 ** 2
      t356 = log(0.4D1 * t203 * t204 * t185 * t186 * t350 * t265 * t280)
      t362 = (-t262 * t184 + 0.2D1 * t21 * t341) ** 2
      t365 = 0.1D1 / (-0.2D1 + t327)
      t373 = -0.90D2 * t18 * t19 * t356 * t350 * t362 * t365 + t157 * t3
     #50 * t362 * t365
      t376 = t373 * t163 * t79 / 0.40D2
      t377 = FJET(XB1, XB2, s, t329, -t331, t332, -t333, t347, t376)
      t379 = t163 * t79
      t382 = FJET(XB1, XB2, s, -t267, t260, 0.0D0, -t268, t274, t324)
      t384 = FJET(XB1, XB2, s, -t331, t329, -t333, t332, t347, t376)
      rrgg2gght15s1e1 = t182 * t181 + t251 * t250 + t253 * t181 + t255 *
     # t250 + t325 * t324 + t377 * t373 * t379 / 0.40D2 + t382 * t324 + 
     #t384 * t373 * t379 / 0.40D2

      end function



      doubleprecision function rrgg2gght15s1e0
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
      t15 = t11 ** 2
      t16 = wd * t15
      t17 = x1 * t6
      t18 = t16 * t17
      t19 = 0.1D1 / z
      t20 = x2 * pi
      t21 = cos(t20)
      t22 = t21 ** 2
      t23 = t19 * t22
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
      t51 = t16 * t17 * lh
      t56 = -0.180D3 * t16 * x1 * t6 * t19 * t22 + (-0.180D3 * t51 + 0.1
     #80D3 * t18) * t19 * t22
      t58 = t56 * t33 * t40
      t60 = 0.1D1 / x4
      t64 = t16 * t17 * t19
      t65 = t33 * t40
      t66 = 0.1D1 / x3
      t75 = t29 * t31 * t33
      t78 = log(0.4D1 * x3 * t25 * t27 * t75)
      t96 = log(0.4D1 * t28 * t75)
      t98 = t96 * wd * t15
      t100 = (0.2D1 * t16 - t98) * x1
      t105 = t65 * t22
      t108 = pi ** 2
      t110 = lh ** 2
      t120 = t96 ** 2
      t132 = -(0.90D2 * t18 * t23 * t37 * t33 * t40 - t58) * t60 / 0.10D
     #2 + 0.9D1 * t64 * t65 * t22 * t66 * t60 - (0.90D2 * t18 * t23 * t7
     #8 * t33 * t40 - t58) * t66 / 0.10D2 + 0.9D1 * t18 * t19 * t33 * t4
     #0 * t22 - (-0.180D3 * t51 + 0.90D2 * t100 * t6) * t19 * t105 / 0.5
     #D1 + (t16 * t17 * (-0.30D2 * t108 + 0.180D3 * t110) - 0.180D3 * t1
     #00 * t6 * lh + 0.90D2 * (0.3D1 * t16 - 0.2D1 * t98 + t120 * wd * t
     #15 / 0.2D1) * x1 * t6) * t19 * t105 / 0.10D2
      t133 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t132)
      t135 = sqrt(x4)
      t136 = -0.1D1 + t135
      t137 = t135 + 0.1D1
      t138 = t136 * t137
      t139 = KAPPA2(x1, x2, 0.0D0, -t138, z)
      t140 = s * t139
      t141 = t140 * t4
      t144 = t6 * t136 * t137
      t145 = t140 * t3 * t144
      t146 = t7 * x4
      t147 = t140 * t146
      t148 = t139 ** 2
      t153 = s * t148 * t11 * t17 * (-0.1D1 + x4)
      t154 = Sqrt(-t138)
      t155 = t154 ** 2
      t157 = 0.1D1 / (-0.2D1 + t139)
      t158 = t155 * t157
      t160 = t148 ** 2
      t167 = t31 * t25
      t173 = log(-0.4D1 * x4 * t29 * t167 * t138 * t27 * t160)
      t185 = -0.9D1 * t64 * t158 * t22 * t160 * t66 * t60 - (-0.90D2 * t
     #64 * t22 * t173 * t158 * t160 + t56 * t155 * t157 * t160) * t60 / 
     #0.10D2
      t186 = FJET(XB1, XB2, s, 0.0D0, t141, t145, -t147, t153, t185)
      t188 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t132)
      t190 = FJET(XB1, XB2, s, t141, 0.0D0, -t147, t145, t153, t185)
      t192 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t193 = s * t192
      t194 = t4 * x3
      t195 = t193 * t194
      t197 = sqrt(x3)
      t198 = -0.1D1 + t197
      t200 = t197 + 0.1D1
      t201 = x1 * t198 * t200
      t202 = t193 * t3 * t201
      t203 = t193 * t7
      t204 = t192 ** 2
      t209 = s * t204 * t11 * t17 * (-0.1D1 + x3)
      t210 = t204 ** 2
      t212 = 0.1D1 / (-0.2D1 + t192)
      t213 = t210 * t212
      t215 = t198 * t200
      t216 = Sqrt(-t215)
      t217 = t216 ** 2
      t229 = log(-0.4D1 * t210 * t29 * t167 * t215 * t27 * x3)
      t241 = -0.9D1 * t64 * t213 * t22 * t217 * t66 * t60 - (-0.90D2 * t
     #64 * t22 * t229 * t213 * t217 + t56 * t210 * t212 * t217) * t66 / 
     #0.10D2
      t242 = FJET(XB1, XB2, s, t195, -t202, -t203, 0.0D0, t209, t241)
      t244 = KAPPA2(x1, x2, x3, -t138, z)
      t245 = s * t244
      t246 = t245 * t194
      t247 = t245 * t3
      t248 = t247 * t201
      t249 = t247 * t144
      t250 = t245 * t146
      t251 = t244 ** 2
      t258 = Sqrt(t215 * t138)
      t264 = s * t251 * t11 * t17 * (x3 - 0.1D1 + x4 - 0.2D1 * x3 * x4 +
     # 0.2D1 * t21 * t197 * t135 * t258)
      t265 = t251 ** 2
      t270 = (-t197 * t135 + 0.2D1 * t21 * t258) ** 2
      t275 = 0.1D1 / (-0.2D1 + t244) * t66 * t60
      t278 = 0.9D1 / 0.4D1 * t64 * t265 * t270 * t275
      t279 = FJET(XB1, XB2, s, t246, -t248, t249, -t250, t264, t278)
      t282 = t15 * x1 * t6
      t286 = t19 * t265 * t270 * t275
      t289 = FJET(XB1, XB2, s, -t202, t195, 0.0D0, -t203, t209, t241)
      t291 = FJET(XB1, XB2, s, -t248, t246, -t250, t249, t264, t278)
      rrgg2gght15s1e0 = t133 * t132 + t185 * t186 + t188 * t132 + t190 *
     # t185 + t242 * t241 + 0.9D1 / 0.4D1 * t279 * wd * t282 * t286 + t2
     #89 * t241 + 0.9D1 / 0.4D1 * t291 * wd * t282 * t286

      end function



      doubleprecision function rrgg2gght15s1em1
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
      t15 = t11 ** 2
      t16 = wd * t15
      t17 = x1 * t6
      t18 = t16 * t17
      t19 = 0.1D1 / z
      t20 = t9 ** 2
      t21 = t19 * t20
      t23 = 0.1D1 / (-0.2D1 + t1)
      t24 = x2 * pi
      t25 = cos(t24)
      t26 = t25 ** 2
      t27 = t23 * t26
      t28 = 0.1D1 / x3
      t40 = sin(t24)
      t41 = t40 ** 2
      t42 = z ** 2
      t45 = x1 ** 2
      t46 = t6 ** 2
      t51 = log(0.4D1 * t41 / t42 * t45 * t46 * t20)
      t64 = 0.1D1 / x4
      t69 = 0.9D1 * t18 * t21 * t27 * t28 - 0.18D2 * t18 * t21 * t27 + (
     #-0.180D3 * t16 * t17 * lh + 0.90D2 * (0.2D1 * t16 - t51 * wd * t15
     #) * x1 * t6) * t19 * t20 * t23 * t26 / 0.10D2 + 0.9D1 * t18 * t21 
     #* t27 * t64
      t70 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t69)
      t72 = sqrt(x4)
      t73 = -0.1D1 + t72
      t74 = t72 + 0.1D1
      t75 = t73 * t74
      t76 = KAPPA2(x1, x2, 0.0D0, -t75, z)
      t77 = s * t76
      t78 = t77 * t4
      t82 = t77 * t3 * t6 * t73 * t74
      t84 = t77 * t7 * x4
      t85 = t76 ** 2
      t90 = s * t85 * t11 * t17 * (-0.1D1 + x4)
      t92 = t16 * t17 * t19
      t93 = Sqrt(-t75)
      t94 = t93 ** 2
      t96 = 0.1D1 / (-0.2D1 + t76)
      t98 = t85 ** 2
      t100 = t26 * t98 * t64
      t103 = 0.9D1 * t92 * t94 * t96 * t100
      t104 = FJET(XB1, XB2, s, 0.0D0, t78, t82, -t84, t90, -t103)
      t107 = t15 * x1 * t6
      t111 = t19 * t94 * t96 * t100
      t114 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t69)
      t116 = FJET(XB1, XB2, s, t78, 0.0D0, -t84, t82, t90, -t103)
      t121 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t122 = s * t121
      t124 = t122 * t4 * x3
      t126 = sqrt(x3)
      t127 = -0.1D1 + t126
      t129 = t126 + 0.1D1
      t131 = t122 * t3 * x1 * t127 * t129
      t132 = t122 * t7
      t133 = t121 ** 2
      t138 = s * t133 * t11 * t17 * (-0.1D1 + x3)
      t139 = t133 ** 2
      t141 = 0.1D1 / (-0.2D1 + t121)
      t144 = Sqrt(-t127 * t129)
      t145 = t144 ** 2
      t147 = t26 * t145 * t28
      t150 = 0.9D1 * t92 * t139 * t141 * t147
      t151 = FJET(XB1, XB2, s, t124, -t131, -t132, 0.0D0, t138, -t150)
      t156 = t19 * t139 * t141 * t147
      t159 = FJET(XB1, XB2, s, -t131, t124, 0.0D0, -t132, t138, -t150)
      rrgg2gght15s1em1 = t70 * t69 - 0.9D1 * t104 * wd * t107 * t111 + t
     #114 * t69 - 0.9D1 * t116 * wd * t107 * t111 - 0.9D1 * t151 * wd * 
     #t107 * t156 - 0.9D1 * t159 * wd * t107 * t156

      end function



      doubleprecision function rrgg2gght15s1em2
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
      t15 = t11 ** 2
      t19 = 0.1D1 / z
      t20 = t9 ** 2
      t23 = 0.1D1 / (-0.2D1 + t1)
      t25 = cos(x2 * pi)
      t26 = t25 ** 2
      t30 = 0.9D1 * wd * t15 * x1 * t6 * t19 * t20 * t23 * t26
      t31 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t30)
      t33 = t15 * x1
      t38 = t6 * t19 * t20 * t23 * t26
      t40 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t30)
      rrgg2gght15s1em2 = 0.9D1 * t31 * wd * t33 * t38 + 0.9D1 * t40 * wd
     # * t33 * t38

      end function



      doubleprecision function rrgg2gght15s1em3
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
      rrgg2gght15s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2gght15s1em4
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
      rrgg2gght15s1em4 = 0.0D0

      end function
