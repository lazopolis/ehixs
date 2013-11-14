  
      subroutine rrgg2qqbarht11
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2qqbarht11s1e1  
      doubleprecision rrgg2qqbarht11s1e0  
      doubleprecision rrgg2qqbarht11s1em1  
      doubleprecision rrgg2qqbarht11s1em2  
      doubleprecision rrgg2qqbarht11s1em3  
      doubleprecision rrgg2qqbarht11s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht11s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht11s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht11s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht11s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht11s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht11s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2qqbarht11s1e1
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
      t2 = s * t1
      t3 = x2 * x3
      t4 = 0.2D1 * t3
      t5 = x4 * pi
      t6 = cos(t5)
      t7 = -0.1D1 + x2
      t9 = -0.1D1 + x3
      t10 = x3 * t9
      t12 = Sqrt(x2 * t7 * t10)
      t14 = 0.2D1 * t6 * t12
      t16 = t2 * (0.1D1 - x2 - x3 + t4 + t14)
      t18 = t2 * (-x3 + t4 - x2 + t14)
      t19 = wd * nf
      t20 = x2 * z
      t22 = (t20 + 0.1D1 - x2) ** 2
      t23 = 0.1D1 / t22
      t24 = t19 * t23
      t25 = t1 ** 2
      t26 = z * t25
      t27 = Sin(t5)
      t28 = t27 ** 2
      t29 = x2 * t28
      t30 = z ** 2
      t31 = 0.1D1 / t30
      t36 = log(0.4D1 * t29 * t31 * t10 * t7)
      t37 = t36 ** 2
      t38 = -x2 + t20 - 0.1D1 + x3
      t43 = t23 * z
      t44 = t19 * t43
      t45 = lh * t25
      t50 = lh ** 2
      t52 = pi ** 2
      t54 = 0.180D3 * t50 - 0.30D2 * t52
      t55 = z * t54
      t61 = 0.1D1 / x3
      t64 = t19 * z
      t65 = x1 ** 2
      t66 = t3 * t65
      t67 = t28 * t31
      t72 = log(0.4D1 * t66 * t67 * t7 * t9)
      t74 = t23 * t38
      t83 = 0.1D1 / x1
      t86 = -(-0.45D2 * t24 * t26 * t37 * t38 - 0.180D3 * t44 * t45 * t3
     #6 * t38 - t24 * t55 * t25 * t38) * t61 / 0.2160D4 - (0.90D2 * t64 
     #* t25 * t72 * t74 + 0.180D3 * t64 * t45 * t74) * t61 * t83 / 0.108
     #0D4
      t87 = FJET(XB1, XB2, s, 0.0D0, t16, 0.0D0, -t18, 0.0D0, t86)
      t90 = x2 * t1 * s
      t92 = t7 * t1 * s
      t93 = t67 * t7
      t96 = log(-0.4D1 * t3 * t93)
      t97 = t96 ** 2
      t98 = -x2 + t20 - 0.1D1
      t109 = t24 * t55 * t25 * t98
      t120 = log(-0.4D1 * t29 * t31 * t7)
      t122 = t120 * wd * nf
      t124 = (0.2D1 * t19 - t122) * t23
      t127 = z * lh
      t132 = t120 ** 2
      t134 = t132 * wd * nf
      t137 = (0.3D1 * t19 - 0.2D1 * t122 + t134 / 0.2D1) * t23
      t170 = log(-0.4D1 * t66 * t93)
      t172 = t23 * t98
      t183 = x2 * t65
      t186 = log(-0.4D1 * t183 * t93)
      t187 = t186 ** 2
      t192 = t19 * t127
      t200 = -(0.45D2 * t24 * t26 * t97 * t98 + 0.180D3 * t44 * t45 * t9
     #6 * t98 + t109) * t61 / 0.2160D4 - (-0.180D3 * t19 * t43 * lh + 0.
     #90D2 * t124 * z + 0.360D3 * t124 * t127 - 0.180D3 * t137 * z - 0.2
     #D1 * t19 * t43 * t54 - 0.180D3 * t137 * t127 + t19 * t43 * (0.60D2
     # * lh * t52 - 0.240D3 * zeta3 - 0.120D3 * t50 * lh) + 0.90D2 * (0.
     #4D1 * t19 - 0.3D1 * t122 + t134 - t132 * t120 * wd * nf / 0.6D1) *
     # t23 * z + t124 * t55) * t98 * t25 / 0.2160D4 - (-0.90D2 * t64 * t
     #25 * t170 * t172 - 0.180D3 * t64 * t45 * t172) * t61 * t83 / 0.108
     #0D4 - (0.45D2 * t64 * t25 * t187 * t172 + 0.180D3 * t192 * t25 * t
     #186 * t172 + t109) * t83 / 0.1080D4
      t201 = FJET(XB1, XB2, s, 0.0D0, t90, 0.0D0, -t92, 0.0D0, t200)
      t203 = FJET(XB1, XB2, s, 0.0D0, -t18, 0.0D0, t16, 0.0D0, t86)
      t205 = FJET(XB1, XB2, s, 0.0D0, -t92, 0.0D0, t90, 0.0D0, t200)
      t207 = -0.1D1 + x1
      t209 = x1 * z
      t210 = 0.1D1 - x1 + t209
      t211 = 0.1D1 / t210
      t213 = t2 * t207 * x2 * t211
      t214 = t2 * x1
      t217 = t7 * s * t1 * t207
      t222 = s * t25 * x2 * x1 * t207 * t211
      t223 = t19 * t26
      t225 = t3 * t65 * t28
      t226 = t31 * t211
      t227 = t207 ** 2
      t228 = t227 * t7
      t229 = t226 * t228
      t232 = log(-0.4D1 * t225 * t229)
      t234 = x2 * x1
      t235 = t234 * z
      t236 = x2 - t234 - t20 + t235 + 0.1D1 - x1 + t209
      t238 = (-t20 + t235 - t209 - 0.1D1 + x1 + x2 - t234) ** 2
      t239 = 0.1D1 / t238
      t240 = t236 * t239
      t244 = t25 * t207
      t245 = t244 * t240
      t254 = log(-0.4D1 * t183 * t28 * t229)
      t255 = t254 ** 2
      t271 = -(0.90D2 * t223 * t232 * t207 * t240 + 0.180D3 * t192 * t24
     #5) * t61 * t83 / 0.1080D4 - (-0.45D2 * t223 * t255 * t207 * t240 -
     # 0.180D3 * t192 * t25 * t254 * t207 * t236 * t239 - t19 * t55 * t2
     #45) * t83 / 0.1080D4
      t272 = FJET(XB1, XB2, s, 0.0D0, -t213, t214, t217, -t222, t271)
      t274 = FJET(XB1, XB2, s, t214, t217, 0.0D0, -t213, -t222, t271)
      t276 = FJET(XB1, XB2, s, t16, 0.0D0, -t18, 0.0D0, 0.0D0, t86)
      t278 = FJET(XB1, XB2, s, t90, 0.0D0, -t92, 0.0D0, 0.0D0, t200)
      t280 = x3 * x1
      t281 = t2 * t280
      t282 = t280 * z
      t283 = t3 * x1
      t284 = t3 * t209
      t289 = Sqrt(x3 * t7 * t210 * x2 * t9)
      t291 = 0.2D1 * t6 * t289
      t295 = t2 * t207 * (-x3 + t280 - t282 + t4 - t283 + t284 - x2 + t2
     #91) * t211
      t298 = t9 * s * t1 * x1
      t299 = 0.1D1 - x1 + t209 - x2 + t234 - t235 - x3 + t280 - t282 + t
     #4 - t283 + t284 + t291
      t302 = t2 * t207 * t299 * t211
      t307 = log(0.4D1 * t225 * t226 * t228 * t9)
      t310 = (x2 - t234 - t20 + t235 + 0.1D1 - x1 + t209 - x3 + t280 - t
     #282) * t239
      t317 = -0.90D2 * t223 * t307 * t207 * t310 - 0.180D3 * t192 * t244
     # * t310
      t320 = t317 * t61 * t83 / 0.1080D4
      t321 = FJET(XB1, XB2, s, t281, t295, -t298, -t302, -t222, -t320)
      t323 = t61 * t83
      t326 = FJET(XB1, XB2, s, t217, t214, -t213, 0.0D0, -t222, t271)
      t328 = FJET(XB1, XB2, s, t295, t281, -t302, -t298, -t222, -t320)
      t332 = FJET(XB1, XB2, s, -t18, 0.0D0, t16, 0.0D0, 0.0D0, t86)
      t334 = FJET(XB1, XB2, s, -t92, 0.0D0, t90, 0.0D0, 0.0D0, t200)
      t336 = FJET(XB1, XB2, s, -t298, -t302, t281, t295, -t222, -t320)
      t340 = FJET(XB1, XB2, s, -t213, 0.0D0, t217, t214, -t222, t271)
      t342 = FJET(XB1, XB2, s, -t302, -t298, t295, t281, -t222, -t320)
      rrgg2qqbarht11s1e1 = t87 * t86 + t201 * t200 + t203 * t86 + t205 *
     # t200 + t272 * t271 + t274 * t271 + t276 * t86 + t278 * t200 - t32
     #1 * t317 * t323 / 0.1080D4 + t326 * t271 - t328 * t317 * t323 / 0.
     #1080D4 + t332 * t86 + t334 * t200 - t336 * t317 * t323 / 0.1080D4 
     #+ t340 * t271 - t342 * t317 * t323 / 0.1080D4

      end function



      doubleprecision function rrgg2qqbarht11s1e0
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
      t2 = s * t1
      t3 = x2 * x3
      t4 = 0.2D1 * t3
      t5 = x4 * pi
      t6 = cos(t5)
      t7 = -0.1D1 + x2
      t9 = -0.1D1 + x3
      t10 = x3 * t9
      t12 = Sqrt(x2 * t7 * t10)
      t14 = 0.2D1 * t6 * t12
      t16 = t2 * (0.1D1 - x2 - x3 + t4 + t14)
      t18 = t2 * (-x3 + t4 - x2 + t14)
      t19 = wd * nf
      t20 = x2 * z
      t22 = (t20 + 0.1D1 - x2) ** 2
      t23 = 0.1D1 / t22
      t24 = t19 * t23
      t25 = t1 ** 2
      t26 = z * t25
      t27 = Sin(t5)
      t28 = t27 ** 2
      t29 = x2 * t28
      t30 = z ** 2
      t31 = 0.1D1 / t30
      t36 = log(0.4D1 * t29 * t31 * t10 * t7)
      t37 = -x2 + t20 - 0.1D1 + x3
      t42 = z * lh
      t49 = 0.1D1 / x3
      t52 = t19 * t26
      t54 = 0.1D1 / x1
      t55 = t49 * t54
      t59 = -(0.90D2 * t24 * t26 * t36 * t37 + 0.180D3 * t24 * t42 * t25
     # * t37) * t49 / 0.2160D4 + t52 * t23 * t37 * t55 / 0.12D2
      t60 = FJET(XB1, XB2, s, 0.0D0, t16, 0.0D0, -t18, 0.0D0, t59)
      t63 = x2 * t1 * s
      t65 = t7 * t1 * s
      t67 = t28 * t31 * t7
      t70 = log(-0.4D1 * t3 * t67)
      t71 = -x2 + t20 - 0.1D1
      t76 = t19 * z
      t78 = t23 * t71
      t81 = 0.180D3 * t76 * lh * t25 * t78
      t88 = x1 ** 2
      t89 = x2 * t88
      t92 = log(-0.4D1 * t89 * t67)
      t100 = t23 * z
      t107 = t31 * t7
      t110 = log(-0.4D1 * t29 * t107)
      t112 = t110 * wd * nf
      t114 = (0.2D1 * t19 - t112) * t23
      t121 = t110 ** 2
      t129 = lh ** 2
      t131 = pi ** 2
      t140 = -(-0.90D2 * t24 * t26 * t70 * t71 - t81) * t49 / 0.2160D4 -
     # t52 * t78 * t55 / 0.12D2 - (-0.90D2 * t76 * t25 * t92 * t78 - t81
     #) * t54 / 0.1080D4 - (0.90D2 * t19 * t100 + 0.360D3 * t19 * t100 *
     # lh - 0.180D3 * t114 * z - 0.180D3 * t114 * t42 + 0.90D2 * (0.3D1 
     #* t19 - 0.2D1 * t112 + t121 * wd * nf / 0.2D1) * t23 * z + t19 * t
     #100 * (0.180D3 * t129 - 0.30D2 * t131)) * t71 * t25 / 0.2160D4
      t141 = FJET(XB1, XB2, s, 0.0D0, t63, 0.0D0, -t65, 0.0D0, t140)
      t143 = FJET(XB1, XB2, s, 0.0D0, -t18, 0.0D0, t16, 0.0D0, t59)
      t145 = FJET(XB1, XB2, s, 0.0D0, -t65, 0.0D0, t63, 0.0D0, t140)
      t147 = -0.1D1 + x1
      t149 = x1 * z
      t150 = 0.1D1 - x1 + t149
      t151 = 0.1D1 / t150
      t153 = t2 * t147 * x2 * t151
      t154 = t2 * x1
      t157 = t7 * s * t1 * t147
      t162 = s * t25 * x2 * x1 * t147 * t151
      t163 = x2 * x1
      t164 = t163 * z
      t165 = x2 - t163 - t20 + t164 + 0.1D1 - x1 + t149
      t168 = (-t20 + t164 - t149 - 0.1D1 + x1 + x2 - t163) ** 2
      t169 = 0.1D1 / t168
      t171 = t169 * t49 * t54
      t176 = t147 ** 2
      t181 = log(-0.4D1 * t89 * t28 * t107 * t176 * t151)
      t183 = t165 * t169
      t195 = t52 * t147 * t165 * t171 / 0.12D2 - (0.90D2 * t52 * t181 * 
     #t147 * t183 + 0.180D3 * t19 * t42 * t25 * t147 * t183) * t54 / 0.1
     #080D4
      t196 = FJET(XB1, XB2, s, 0.0D0, -t153, t154, t157, -t162, t195)
      t198 = FJET(XB1, XB2, s, t154, t157, 0.0D0, -t153, -t162, t195)
      t200 = FJET(XB1, XB2, s, t16, 0.0D0, -t18, 0.0D0, 0.0D0, t59)
      t202 = FJET(XB1, XB2, s, t63, 0.0D0, -t65, 0.0D0, 0.0D0, t140)
      t204 = x3 * x1
      t205 = t2 * t204
      t206 = t204 * z
      t207 = t3 * x1
      t208 = t3 * t149
      t213 = Sqrt(x3 * t7 * t150 * x2 * t9)
      t215 = 0.2D1 * t6 * t213
      t219 = t2 * t147 * (-x3 + t204 - t206 + t4 - t207 + t208 - x2 + t2
     #15) * t151
      t222 = t9 * s * t1 * x1
      t223 = 0.1D1 - x1 + t149 - x2 + t163 - t164 - x3 + t204 - t206 + t
     #4 - t207 + t208 + t215
      t226 = t2 * t147 * t223 * t151
      t229 = t147 * (x2 - t163 - t20 + t164 + 0.1D1 - x1 + t149 - x3 + t
     #204 - t206) * t171
      t231 = t52 * t229 / 0.12D2
      t232 = FJET(XB1, XB2, s, t205, t219, -t222, -t226, -t162, -t231)
      t235 = nf * z * t25
      t239 = FJET(XB1, XB2, s, t157, t154, -t153, 0.0D0, -t162, t195)
      t241 = FJET(XB1, XB2, s, t219, t205, -t226, -t222, -t162, -t231)
      t246 = FJET(XB1, XB2, s, -t18, 0.0D0, t16, 0.0D0, 0.0D0, t59)
      t248 = FJET(XB1, XB2, s, -t65, 0.0D0, t63, 0.0D0, 0.0D0, t140)
      t250 = FJET(XB1, XB2, s, -t222, -t226, t205, t219, -t162, -t231)
      t255 = FJET(XB1, XB2, s, -t153, 0.0D0, t157, t154, -t162, t195)
      t257 = FJET(XB1, XB2, s, -t226, -t222, t219, t205, -t162, -t231)
      rrgg2qqbarht11s1e0 = t60 * t59 + t141 * t140 + t143 * t59 + t145 *
     # t140 + t196 * t195 + t198 * t195 + t200 * t59 + t202 * t140 - t23
     #2 * wd * t235 * t229 / 0.12D2 + t239 * t195 - t241 * wd * t235 * t
     #229 / 0.12D2 + t246 * t59 + t248 * t140 - t250 * wd * t235 * t229 
     #/ 0.12D2 + t255 * t195 - t257 * wd * t235 * t229 / 0.12D2

      end function



      doubleprecision function rrgg2qqbarht11s1em1
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
      t2 = s * t1
      t4 = 0.2D1 * x2 * x3
      t5 = x4 * pi
      t6 = cos(t5)
      t7 = -0.1D1 + x2
      t12 = Sqrt(x2 * t7 * x3 * (-0.1D1 + x3))
      t14 = 0.2D1 * t6 * t12
      t16 = t2 * (0.1D1 - x2 - x3 + t4 + t14)
      t18 = t2 * (-x3 + t4 - x2 + t14)
      t19 = wd * nf
      t20 = x2 * z
      t22 = (t20 + 0.1D1 - x2) ** 2
      t23 = 0.1D1 / t22
      t25 = t1 ** 2
      t26 = z * t25
      t28 = 0.1D1 / x3
      t30 = t26 * (x2 - t20 - x3 + 0.1D1) * t28
      t32 = t19 * t23 * t30 / 0.24D2
      t33 = FJET(XB1, XB2, s, 0.0D0, t16, 0.0D0, -t18, 0.0D0, -t32)
      t35 = nf * t23
      t40 = x2 * t1 * s
      t42 = t7 * t1 * s
      t43 = t19 * z
      t44 = t25 * t23
      t45 = -x2 + t20 - 0.1D1
      t46 = 0.1D1 / x1
      t51 = t23 * z
      t58 = Sin(t5)
      t59 = t58 ** 2
      t61 = z ** 2
      t66 = log(-0.4D1 * x2 * t59 / t61 * t7)
      t81 = -t43 * t44 * t45 * t46 / 0.12D2 - (-0.180D3 * t19 * t51 - 0.
     #180D3 * t19 * t51 * lh + 0.90D2 * (0.2D1 * t19 - t66 * wd * nf) * 
     #t23 * z) * t45 * t25 / 0.2160D4 - t43 * t44 * t45 * t28 / 0.24D2
      t82 = FJET(XB1, XB2, s, 0.0D0, t40, 0.0D0, -t42, 0.0D0, t81)
      t84 = FJET(XB1, XB2, s, 0.0D0, -t18, 0.0D0, t16, 0.0D0, -t32)
      t89 = FJET(XB1, XB2, s, 0.0D0, -t42, 0.0D0, t40, 0.0D0, t81)
      t91 = -0.1D1 + x1
      t93 = x1 * z
      t95 = 0.1D1 / (0.1D1 - x1 + t93)
      t97 = t2 * t91 * x2 * t95
      t98 = t2 * x1
      t101 = t7 * s * t1 * t91
      t106 = s * t25 * x2 * x1 * t91 * t95
      t108 = x2 * x1
      t109 = t108 * z
      t110 = x2 - t108 - t20 + t109 + 0.1D1 - x1 + t93
      t113 = (-t20 + t109 - t93 - 0.1D1 + x1 + x2 - t108) ** 2
      t114 = 0.1D1 / t113
      t118 = t19 * t26 * t91 * t110 * t114 * t46 / 0.12D2
      t119 = FJET(XB1, XB2, s, 0.0D0, -t97, t98, t101, -t106, t118)
      t121 = nf * z
      t126 = t25 * t91 * t110 * t114 * t46
      t129 = FJET(XB1, XB2, s, t98, t101, 0.0D0, -t97, -t106, t118)
      t134 = FJET(XB1, XB2, s, t16, 0.0D0, -t18, 0.0D0, 0.0D0, -t32)
      t139 = FJET(XB1, XB2, s, t40, 0.0D0, -t42, 0.0D0, 0.0D0, t81)
      t141 = FJET(XB1, XB2, s, t101, t98, -t97, 0.0D0, -t106, t118)
      t146 = FJET(XB1, XB2, s, -t18, 0.0D0, t16, 0.0D0, 0.0D0, -t32)
      t151 = FJET(XB1, XB2, s, -t42, 0.0D0, t40, 0.0D0, 0.0D0, t81)
      t153 = FJET(XB1, XB2, s, -t97, 0.0D0, t101, t98, -t106, t118)
      rrgg2qqbarht11s1em1 = -t33 * wd * t35 * t30 / 0.24D2 + t82 * t81 -
     # t84 * wd * t35 * t30 / 0.24D2 + t89 * t81 + t119 * wd * t121 * t1
     #26 / 0.12D2 + t129 * wd * t121 * t126 / 0.12D2 - t134 * wd * t35 *
     # t30 / 0.24D2 + t139 * t81 + t141 * wd * t121 * t126 / 0.12D2 - t1
     #46 * wd * t35 * t30 / 0.24D2 + t151 * t81 + t153 * wd * t121 * t12
     #6 / 0.12D2

      end function



      doubleprecision function rrgg2qqbarht11s1em2
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
      t3 = x2 * t1 * s
      t6 = (-0.1D1 + x2) * t1 * s
      t9 = t1 ** 2
      t10 = x2 * z
      t12 = (t10 + 0.1D1 - x2) ** 2
      t13 = 0.1D1 / t12
      t15 = -x2 + t10 - 0.1D1
      t18 = wd * nf * z * t9 * t13 * t15 / 0.24D2
      t19 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t6, 0.0D0, -t18)
      t24 = z * t9 * t13 * t15
      t26 = FJET(XB1, XB2, s, 0.0D0, -t6, 0.0D0, t3, 0.0D0, -t18)
      t30 = FJET(XB1, XB2, s, t3, 0.0D0, -t6, 0.0D0, 0.0D0, -t18)
      t34 = FJET(XB1, XB2, s, -t6, 0.0D0, t3, 0.0D0, 0.0D0, -t18)
      rrgg2qqbarht11s1em2 = -t19 * wd * nf * t24 / 0.24D2 - t26 * wd * n
     #f * t24 / 0.24D2 - t30 * wd * nf * t24 / 0.24D2 - t34 * wd * nf * 
     #t24 / 0.24D2

      end function



      doubleprecision function rrgg2qqbarht11s1em3
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
      rrgg2qqbarht11s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarht11s1em4
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
      rrgg2qqbarht11s1em4 = 0.0D0

      end function
