  
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
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x2
      t4 = -0.1D1 + x2
      t5 = t2 * t4
      t6 = wd * nf
      t7 = t1 ** 2
      t8 = t6 * t7
      t9 = x2 * z
      t11 = (0.1D1 + t9 - x2) ** 2
      t12 = 0.1D1 / t11
      t13 = z * t12
      t14 = x2 * x3
      t15 = x4 * 0.3141592653589793D1
      t16 = Sin(t15)
      t17 = t16 ** 2
      t18 = z ** 2
      t19 = 0.1D1 / t18
      t20 = t17 * t19
      t21 = t20 * t4
      t24 = log(-0.4D1 * t14 * t21)
      t25 = t24 ** 2
      t26 = -x2 + t9 - 0.1D1
      t31 = t7 * z
      t32 = t6 * t31
      t33 = lh * t12
      t38 = lh ** 2
      t40 = 0.3141592653589793D1 ** 2
      t42 = 0.180D3 * t38 - 0.30D2 * t40
      t43 = z * t42
      t44 = t12 * t26
      t46 = t8 * t43 * t44
      t48 = 0.1D1 / x3
      t52 = x2 * t17
      t56 = log(-0.4D1 * t52 * t19 * t4)
      t58 = t56 * wd * nf
      t60 = (0.2D1 * t6 - t58) * t7
      t64 = t56 ** 2
      t66 = t64 * wd * nf
      t69 = (0.3D1 * t6 - 0.2D1 * t58 + t66 / 0.2D1) * t7
      t101 = x1 ** 2
      t102 = t14 * t101
      t105 = log(-0.4D1 * t102 * t21)
      t110 = z * lh
      t116 = 0.1D1 / x1
      t119 = x2 * t101
      t122 = log(-0.4D1 * t119 * t21)
      t123 = t122 ** 2
      t135 = -(0.45D2 * t8 * t13 * t25 * t26 + 0.180D3 * t32 * t33 * t24
     # * t26 + t46) * t48 / 0.2160D4 - (-0.180D3 * (t8 - 0.2D1 * t60 + t
     #69) * z * lh + t6 * t31 * (0.60D2 * lh * t40 - 0.2884936567583026D
     #3 - 0.120D3 * t38 * lh) + 0.90D2 * (t60 - 0.2D1 * t69 + (0.4D1 * t
     #6 - 0.3D1 * t58 + t66 - t64 * t56 * wd * nf / 0.6D1) * t7) * z + (
     #-0.2D1 * t8 + t60) * z * t42) * t12 * t26 / 0.2160D4 + (0.90D2 * t
     #8 * z * t105 * t44 + 0.180D3 * t8 * t110 * t44) * t48 * t116 / 0.1
     #080D4 + (-0.45D2 * t8 * z * t123 * t44 - 0.180D3 * t32 * lh * t122
     # * t44 - t46) * t116 / 0.1080D4
      t136 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t135)
      t138 = 0.2D1 * t14
      t139 = cos(t15)
      t141 = -0.1D1 + x3
      t142 = x3 * t141
      t144 = Sqrt(x2 * t4 * t142)
      t146 = 0.2D1 * t139 * t144
      t148 = t2 * (0.1D1 - x2 - x3 + t138 + t146)
      t150 = t2 * (-x3 + t138 - x2 + t146)
      t155 = log(0.4D1 * t52 * t19 * t142 * t4)
      t156 = t155 ** 2
      t157 = -x2 + t9 - 0.1D1 + x3
      t161 = 0.45D2 * t8 * t13 * t156 * t157
      t165 = 0.180D3 * t32 * t33 * t155 * t157
      t177 = log(0.4D1 * t102 * t20 * t4 * t141)
      t179 = t157 * t12
      t189 = (-0.90D2 * t8 * z * t177 * t179 - 0.180D3 * t8 * t110 * t17
     #9) * t48 * t116 / 0.1080D4
      t190 = -(-t161 - t165 - t8 * t43 * t157 * t12) * t48 / 0.2160D4 + 
     #t189
      t191 = FJET(XB1, XB2, s, 0.0D0, t148, 0.0D0, -t150, 0.0D0, t190)
      t193 = FJET(XB1, XB2, s, 0.0D0, -t5, 0.0D0, t3, 0.0D0, t135)
      t195 = FJET(XB1, XB2, s, 0.0D0, -t150, 0.0D0, t148, 0.0D0, t190)
      t197 = -0.1D1 + x1
      t199 = x1 * z
      t200 = 0.1D1 - x1 + t199
      t201 = 0.1D1 / t200
      t203 = t2 * t197 * x2 * t201
      t204 = t2 * x1
      t207 = t4 * s * t1 * t197
      t212 = s * t7 * x2 * x1 * t197 * t201
      t214 = t14 * t101 * t17
      t215 = t19 * t201
      t216 = t197 ** 2
      t217 = t216 * t4
      t218 = t215 * t217
      t221 = log(-0.4D1 * t214 * t218)
      t223 = x2 * x1
      t224 = t223 * z
      t225 = x2 - t223 - t9 + t224 + 0.1D1 - x1 + t199
      t227 = (-0.1D1 + x1 - t9 + t224 - t199 + x2 - t223) ** 2
      t228 = 0.1D1 / t227
      t229 = t225 * t228
      t233 = lh * t197
      t243 = log(-0.4D1 * t119 * t17 * t218)
      t244 = t243 ** 2
      t261 = (-0.90D2 * t32 * t221 * t197 * t229 - 0.180D3 * t32 * t233 
     #* t229) * t48 * t116 / 0.1080D4 + (0.45D2 * t32 * t244 * t197 * t2
     #29 + 0.180D3 * t32 * lh * t243 * t197 * t225 * t228 + t32 * t42 * 
     #t197 * t229) * t116 / 0.1080D4
      t262 = FJET(XB1, XB2, s, 0.0D0, -t203, t204, t207, -t212, t261)
      t264 = FJET(XB1, XB2, s, t204, t207, 0.0D0, -t203, -t212, t261)
      t266 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t135)
      t268 = FJET(XB1, XB2, s, t148, 0.0D0, -t150, 0.0D0, 0.0D0, t190)
      t270 = x3 * x1
      t271 = t2 * t270
      t272 = t270 * z
      t273 = t14 * x1
      t274 = t14 * t199
      t279 = Sqrt(x3 * t4 * t200 * x2 * t141)
      t281 = 0.2D1 * t139 * t279
      t285 = t2 * t197 * (-x3 + t270 - t272 + t138 - t273 + t274 - x2 + 
     #t281) * t201
      t288 = t141 * s * t1 * x1
      t289 = 0.1D1 - x1 + t199 - x2 + t223 - t224 - x3 + t270 - t272 + t
     #138 - t273 + t274 + t281
      t292 = t2 * t197 * t289 * t201
      t297 = log(0.4D1 * t214 * t215 * t217 * t141)
      t300 = (x2 - t223 - t9 + t224 + 0.1D1 - x1 + t199 - x3 + t270 - t2
     #72) * t228
      t307 = 0.90D2 * t32 * t297 * t197 * t300 + 0.180D3 * t32 * t233 * 
     #t300
      t310 = t307 * t48 * t116 / 0.1080D4
      t311 = FJET(XB1, XB2, s, t271, t285, -t288, -t292, -t212, t310)
      t313 = t48 * t116
      t316 = FJET(XB1, XB2, s, t207, t204, -t203, 0.0D0, -t212, t261)
      t318 = FJET(XB1, XB2, s, t285, t271, -t292, -t288, -t212, t310)
      t322 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, t135)
      t329 = -(-t161 - t165 - t8 * t43 * t179) * t48 / 0.2160D4 + t189
      t330 = FJET(XB1, XB2, s, -t150, 0.0D0, t148, 0.0D0, 0.0D0, t329)
      t332 = FJET(XB1, XB2, s, -t288, -t292, t271, t285, -t212, t310)
      t336 = FJET(XB1, XB2, s, -t203, 0.0D0, t207, t204, -t212, t261)
      t338 = FJET(XB1, XB2, s, -t292, -t288, t285, t271, -t212, t310)
      rrgg2qqbarht11s1e1 = t136 * t135 + t191 * t190 + t193 * t135 + t19
     #5 * t190 + t262 * t261 + t264 * t261 + t266 * t135 + t268 * t190 +
     # t311 * t307 * t313 / 0.1080D4 + t316 * t261 + t318 * t307 * t313 
     #/ 0.1080D4 + t322 * t135 + t330 * t329 + t332 * t307 * t313 / 0.10
     #80D4 + t336 * t261 + t338 * t307 * t313 / 0.1080D4

      end function



      doubleprecision function rrgg2qqbarht11s1e0
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
      t3 = t2 * x2
      t4 = -0.1D1 + x2
      t5 = t2 * t4
      t6 = wd * nf
      t7 = t1 ** 2
      t8 = t6 * t7
      t9 = x2 * z
      t11 = (0.1D1 + t9 - x2) ** 2
      t12 = 0.1D1 / t11
      t13 = z * t12
      t14 = x2 * x3
      t15 = x4 * 0.3141592653589793D1
      t16 = Sin(t15)
      t17 = t16 ** 2
      t18 = z ** 2
      t19 = 0.1D1 / t18
      t21 = t17 * t19 * t4
      t24 = log(-0.4D1 * t14 * t21)
      t25 = -x2 + t9 - 0.1D1
      t30 = z * lh
      t31 = t12 * t25
      t34 = 0.180D3 * t8 * t30 * t31
      t36 = 0.1D1 / x3
      t39 = t7 * z
      t40 = t6 * t39
      t41 = 0.1D1 / x1
      t42 = t36 * t41
      t46 = x1 ** 2
      t47 = x2 * t46
      t50 = log(-0.4D1 * t47 * t21)
      t60 = x2 * t17
      t64 = log(-0.4D1 * t60 * t19 * t4)
      t66 = t64 * wd * nf
      t68 = (0.2D1 * t6 - t66) * t7
      t76 = t64 ** 2
      t85 = lh ** 2
      t87 = 0.3141592653589793D1 ** 2
      t96 = -(-0.90D2 * t8 * t13 * t24 * t25 - t34) * t36 / 0.2160D4 - t
     #40 * t31 * t42 / 0.12D2 + (0.90D2 * t8 * z * t50 * t31 + t34) * t4
     #1 / 0.1080D4 - (-0.180D3 * (-0.2D1 * t8 + t68) * z * lh + 0.90D2 *
     # (t8 - 0.2D1 * t68 + (0.3D1 * t6 - 0.2D1 * t66 + t76 * wd * nf / 0
     #.2D1) * t7) * z + t6 * t39 * (0.180D3 * t85 - 0.30D2 * t87)) * t12
     # * t25 / 0.2160D4
      t97 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t96)
      t99 = 0.2D1 * t14
      t100 = cos(t15)
      t102 = -0.1D1 + x3
      t103 = x3 * t102
      t105 = Sqrt(x2 * t4 * t103)
      t107 = 0.2D1 * t100 * t105
      t109 = t2 * (0.1D1 - x2 - x3 + t99 + t107)
      t111 = t2 * (-x3 + t99 - x2 + t107)
      t116 = log(0.4D1 * t60 * t19 * t103 * t4)
      t117 = -x2 + t9 - 0.1D1 + x3
      t121 = 0.90D2 * t8 * t13 * t116 * t117
      t130 = t117 * t12
      t133 = t40 * t130 * t42 / 0.12D2
      t134 = -(t121 + 0.180D3 * t8 * t30 * t117 * t12) * t36 / 0.2160D4 
     #+ t133
      t135 = FJET(XB1, XB2, s, 0.0D0, t109, 0.0D0, -t111, 0.0D0, t134)
      t137 = FJET(XB1, XB2, s, 0.0D0, -t5, 0.0D0, t3, 0.0D0, t96)
      t139 = FJET(XB1, XB2, s, 0.0D0, -t111, 0.0D0, t109, 0.0D0, t134)
      t141 = -0.1D1 + x1
      t143 = x1 * z
      t144 = 0.1D1 - x1 + t143
      t145 = 0.1D1 / t144
      t147 = t2 * t141 * x2 * t145
      t148 = t2 * x1
      t151 = t4 * s * t1 * t141
      t156 = s * t7 * x2 * x1 * t141 * t145
      t157 = x2 * x1
      t158 = t157 * z
      t159 = x2 - t157 - t9 + t158 + 0.1D1 - x1 + t143
      t162 = (-0.1D1 + x1 - t9 + t158 - t143 + x2 - t157) ** 2
      t163 = 0.1D1 / t162
      t165 = t163 * t36 * t41
      t171 = t141 ** 2
      t176 = log(-0.4D1 * t47 * t17 * t19 * t145 * t171 * t4)
      t178 = t159 * t163
      t189 = t40 * t141 * t159 * t165 / 0.12D2 + (-0.90D2 * t40 * t176 *
     # t141 * t178 - 0.180D3 * t40 * lh * t141 * t178) * t41 / 0.1080D4
      t190 = FJET(XB1, XB2, s, 0.0D0, -t147, t148, t151, -t156, t189)
      t192 = FJET(XB1, XB2, s, t148, t151, 0.0D0, -t147, -t156, t189)
      t194 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t96)
      t196 = FJET(XB1, XB2, s, t109, 0.0D0, -t111, 0.0D0, 0.0D0, t134)
      t198 = x3 * x1
      t199 = t2 * t198
      t200 = t198 * z
      t201 = t14 * x1
      t202 = t14 * t143
      t207 = Sqrt(x3 * t4 * t144 * x2 * t102)
      t209 = 0.2D1 * t100 * t207
      t213 = t2 * t141 * (-x3 + t198 - t200 + t99 - t201 + t202 - x2 + t
     #209) * t145
      t216 = t102 * s * t1 * x1
      t217 = 0.1D1 - x1 + t143 - x2 + t157 - t158 - x3 + t198 - t200 + t
     #99 - t201 + t202 + t209
      t220 = t2 * t141 * t217 * t145
      t223 = t141 * (x2 - t157 - t9 + t158 + 0.1D1 - x1 + t143 - x3 + t1
     #98 - t200) * t165
      t225 = t40 * t223 / 0.12D2
      t226 = FJET(XB1, XB2, s, t199, t213, -t216, -t220, -t156, -t225)
      t229 = nf * t7 * z
      t233 = FJET(XB1, XB2, s, t151, t148, -t147, 0.0D0, -t156, t189)
      t235 = FJET(XB1, XB2, s, t213, t199, -t220, -t216, -t156, -t225)
      t240 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, t96)
      t248 = -(t121 + 0.180D3 * t8 * t30 * t130) * t36 / 0.2160D4 + t133
      t249 = FJET(XB1, XB2, s, -t111, 0.0D0, t109, 0.0D0, 0.0D0, t248)
      t251 = FJET(XB1, XB2, s, -t216, -t220, t199, t213, -t156, -t225)
      t256 = FJET(XB1, XB2, s, -t147, 0.0D0, t151, t148, -t156, t189)
      t258 = FJET(XB1, XB2, s, -t220, -t216, t213, t199, -t156, -t225)
      rrgg2qqbarht11s1e0 = t97 * t96 + t135 * t134 + t137 * t96 + t139 *
     # t134 + t190 * t189 + t192 * t189 + t194 * t96 + t196 * t134 - t22
     #6 * wd * t229 * t223 / 0.12D2 + t233 * t189 - t235 * wd * t229 * t
     #223 / 0.12D2 + t240 * t96 + t249 * t248 - t251 * wd * t229 * t223 
     #/ 0.12D2 + t256 * t189 - t258 * wd * t229 * t223 / 0.12D2

      end function



      doubleprecision function rrgg2qqbarht11s1em1
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
      t3 = t2 * x2
      t4 = -0.1D1 + x2
      t5 = t2 * t4
      t6 = wd * nf
      t7 = t1 ** 2
      t8 = t6 * t7
      t9 = x2 * z
      t11 = (0.1D1 + t9 - x2) ** 2
      t12 = 0.1D1 / t11
      t13 = z * t12
      t14 = -x2 + t9 - 0.1D1
      t15 = 0.1D1 / x1
      t20 = t7 * z
      t26 = x4 * 0.3141592653589793D1
      t27 = Sin(t26)
      t28 = t27 ** 2
      t30 = z ** 2
      t35 = log(-0.4D1 * x2 * t28 / t30 * t4)
      t47 = 0.1D1 / x3
      t52 = -t8 * t13 * t14 * t15 / 0.12D2 - (-0.180D3 * t6 * t20 * lh +
     # 0.90D2 * (-0.2D1 * t8 + (0.2D1 * t6 - t35 * wd * nf) * t7) * z) *
     # t12 * t14 / 0.2160D4 - t8 * t13 * t14 * t47 / 0.24D2
      t53 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t52)
      t56 = 0.2D1 * x2 * x3
      t57 = cos(t26)
      t62 = Sqrt(x2 * t4 * x3 * (-0.1D1 + x3))
      t64 = 0.2D1 * t57 * t62
      t66 = t2 * (0.1D1 - x2 - x3 + t56 + t64)
      t68 = t2 * (-x3 + t56 - x2 + t64)
      t69 = x2 - t9 - x3 + 0.1D1
      t71 = t13 * t69 * t47
      t73 = t8 * t71 / 0.24D2
      t74 = FJET(XB1, XB2, s, 0.0D0, t66, 0.0D0, -t68, 0.0D0, -t73)
      t76 = nf * t7
      t80 = FJET(XB1, XB2, s, 0.0D0, -t5, 0.0D0, t3, 0.0D0, t52)
      t82 = FJET(XB1, XB2, s, 0.0D0, -t68, 0.0D0, t66, 0.0D0, -t73)
      t87 = -0.1D1 + x1
      t89 = x1 * z
      t91 = 0.1D1 / (0.1D1 - x1 + t89)
      t93 = t2 * t87 * x2 * t91
      t94 = t2 * x1
      t97 = t4 * s * t1 * t87
      t102 = s * t7 * x2 * x1 * t87 * t91
      t104 = x2 * x1
      t105 = t104 * z
      t106 = x2 - t104 - t9 + t105 + 0.1D1 - x1 + t89
      t109 = (-0.1D1 + x1 - t9 + t105 - t89 + x2 - t104) ** 2
      t110 = 0.1D1 / t109
      t114 = t6 * t20 * t87 * t106 * t110 * t15 / 0.12D2
      t115 = FJET(XB1, XB2, s, 0.0D0, -t93, t94, t97, -t102, t114)
      t121 = z * t87 * t106 * t110 * t15
      t124 = FJET(XB1, XB2, s, t94, t97, 0.0D0, -t93, -t102, t114)
      t129 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t52)
      t131 = FJET(XB1, XB2, s, t66, 0.0D0, -t68, 0.0D0, 0.0D0, -t73)
      t136 = FJET(XB1, XB2, s, t97, t94, -t93, 0.0D0, -t102, t114)
      t141 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, t52)
      t143 = FJET(XB1, XB2, s, -t93, 0.0D0, t97, t94, -t102, t114)
      t151 = -z * t69 * t12 * t47
      t154 = FJET(XB1, XB2, s, -t68, 0.0D0, t66, 0.0D0, 0.0D0, t8 * t151
     # / 0.24D2)
      rrgg2qqbarht11s1em1 = t53 * t52 - t74 * wd * t76 * t71 / 0.24D2 + 
     #t80 * t52 - t82 * wd * t76 * t71 / 0.24D2 + t115 * wd * t76 * t121
     # / 0.12D2 + t124 * wd * t76 * t121 / 0.12D2 + t129 * t52 - t131 * 
     #wd * t76 * t71 / 0.24D2 + t136 * wd * t76 * t121 / 0.12D2 + t141 *
     # t52 + t143 * wd * t76 * t121 / 0.12D2 + t154 * wd * t76 * t151 / 
     #0.24D2

      end function



      doubleprecision function rrgg2qqbarht11s1em2
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
      t3 = t2 * x2
      t5 = t2 * (-0.1D1 + x2)
      t7 = t1 ** 2
      t9 = x2 * z
      t11 = (0.1D1 + t9 - x2) ** 2
      t12 = 0.1D1 / t11
      t14 = -x2 + t9 - 0.1D1
      t17 = wd * nf * t7 * z * t12 * t14 / 0.24D2
      t18 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, -t17)
      t23 = t7 * z * t12 * t14
      t25 = FJET(XB1, XB2, s, 0.0D0, -t5, 0.0D0, t3, 0.0D0, -t17)
      t29 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, -t17)
      t33 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, -t17)
      rrgg2qqbarht11s1em2 = -t18 * wd * nf * t23 / 0.24D2 - t25 * wd * n
     #f * t23 / 0.24D2 - t29 * wd * nf * t23 / 0.24D2 - t33 * wd * nf * 
     #t23 / 0.24D2

      end function



      doubleprecision function rrgg2qqbarht11s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgg2qqbarht11s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarht11s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgg2qqbarht11s1em4 = 0.0D0

      end function
