

double GstarGstarMeNNLOSoft::TT37 (
  double u,
  double t,
  double q3,
  double q4,
  double z,
  double zp,
  double Nc,
  double Nf,
  double CF)
{
  double t1;
  double t10;
  double t105;
  double t11;
  double t110;
  double t118;
  double t135;
  double t153;
  double t16;
  double t161;
  double t17;
  double t2;
  double t20;
  double t207;
  double t219;
  double t226;
  double t23;
  double t26;
  double t27;
  double t3;
  double t30;
  double t33;
  double t36;
  double t39;
  double t4;
  double t40;
  double t45;
  double t52;
  double t57;
  double t62;
  double t63;
  double t66;
  double t69;
  double t72;
  double t75;
  double t78;
  double t9;
  double t93;
  double t96;
  t1 = q3 * q3;
  t2 = t1 * t1;
  t3 = q4 * q4;
  t4 = t2 * t3;
  t9 = t1 * q3;
  t10 = t3 * q4;
  t11 = t9 * t10;
  t16 = t9 * t3;
  t17 = t * t;
  t20 = u * t;
  t23 = u * u;
  t26 = t9 * q4;
  t27 = t17 * t;
  t30 = t17 * u;
  t33 = t * t23;
  t36 = t23 * u;
  t39 = t3 * t3;
  t40 = t1 * t39;
  t45 = t1 * t10;
  t52 = t1 * t3;
  t57 = -0.8e1 * t * t11 + 0.12e2 * t * t4 + 0.12e2 * t * t40 + 0.120e3 * t11 * u - 0.8e1 * t16 * t17 - 0.96e2 * t16 * t20 - 0.120e3 * t16 * t23 - 0.8e1 * t17 * t45 - 0.96e2 * t20 * t45 - 0.120e3 * t23 * t45 - 0.8e1 * t26 * t27 - 0.48e2 * t26 * t30 - 0.60e2 * t26 * t33 - 0.20e2 * t26 * t36 + 0.12e2 * t27 * t52 - 0.28e2 * t30 * t52 + 0.60e2 * t4 * u + 0.60e2 * t40 * u;
  t62 = t1 * q4;
  t63 = t17 * t17;
  t66 = t27 * u;
  t69 = t17 * t23;
  t72 = t * t36;
  t75 = t23 * t23;
  t78 = t63 * t;
  t93 = t75 * u;
  t96 = q3 * t10;
  t105 = q3 * t3;
  t110 = 0.10e2 * t * t1 * t75 + 0.20e2 * t1 * t17 * t36 + 0.20e2 * t1 * t23 * t27 + 0.10e2 * t1 * t63 * u + 0.2e1 * t1 * t78 + 0.2e1 * t1 * t93 + 0.4e1 * t105 * t63 + 0.68e2 * t105 * t66 - 0.8e1 * t27 * t96 - 0.48e2 * t30 * t96 - 0.20e2 * t33 * t52 - 0.60e2 * t33 * t96 + 0.20e2 * t36 * t52 - 0.20e2 * t36 * t96 + 0.4e1 * t62 * t63 + 0.68e2 * t62 * t66 + 0.164e3 * t62 * t69 + 0.140e3 * t62 * t72 + 0.40e2 * t62 * t75;
  t118 = q3 * q4;
  t135 = t63 * t17;
  t153 = t75 * t23;
  t161 = -0.22e2 * q3 * t * t93 - 0.50e2 * q3 * t17 * t75 - 0.40e2 * q3 * t23 * t63 - 0.60e2 * q3 * t27 * t36 - 0.14e2 * q3 * t78 * u - 0.62e2 * t * t118 * t75 - 0.92e2 * t118 * t17 * t36 - 0.64e2 * t118 * t23 * t27 - 0.20e2 * t118 * t63 * u + 0.10e2 * t3 * t63 * u - 0.2e1 * q3 * t135 - 0.4e1 * q3 * t153 + 0.164e3 * t105 * t69 + 0.140e3 * t105 * t72 + 0.40e2 * t105 * t75 - 0.2e1 * t118 * t78 - 0.16e2 * t118 * t93 + 0.2e1 * t3 * t78;
  t207 = -0.22e2 * q4 * t * t93 - 0.50e2 * q4 * t17 * t75 - 0.40e2 * q4 * t23 * t63 - 0.60e2 * q4 * t27 * t36 - 0.14e2 * q4 * t78 * u + 0.10e2 * t * t3 * t75 + 0.20e2 * t17 * t3 * t36 + 0.20e2 * t23 * t27 * t3 - 0.2e1 * q4 * t135 - 0.4e1 * q4 * t153 + 0.12e2 * t * t153 + 0.7e1 * t135 * u + 0.31e2 * t17 * t93 + 0.22e2 * t23 * t78 + t27 * t63 + 0.45e2 * t27 * t75 + 0.2e1 * t3 * t93 + 0.40e2 * t36 * t63 + 0.2e1 * t36 * t75;
  t219 = pow(0.4e1 * t118 - t17 - 0.2e1 * t20 - t23, 0.2e1);
  t226 = QQoc(0.1e1 - zp, 0.1e1 - z, -u);
  return(0.4e1 / 0.3e1 * (t57 + t110 + t161 + t207) * Nc * CF * Nf / t / u / t219 / (z - zp) * t226);
}
#include <math.h>

double GstarGstarMeNNLOSoft::TT36 (
  double u,
  double t,
  double q3,
  double q4,
  double z,
  double zp,
  double Nc,
  double Nf,
  double CF)
{
  double t1;
  double t10;
  double t105;
  double t11;
  double t110;
  double t118;
  double t135;
  double t153;
  double t16;
  double t161;
  double t17;
  double t2;
  double t20;
  double t207;
  double t217;
  double t224;
  double t23;
  double t26;
  double t27;
  double t3;
  double t30;
  double t33;
  double t36;
  double t39;
  double t4;
  double t40;
  double t45;
  double t52;
  double t57;
  double t62;
  double t63;
  double t66;
  double t69;
  double t72;
  double t75;
  double t78;
  double t9;
  double t93;
  double t96;
  t1 = q3 * q3;
  t2 = t1 * t1;
  t3 = q4 * q4;
  t4 = t2 * t3;
  t9 = t1 * q3;
  t10 = t3 * q4;
  t11 = t9 * t10;
  t16 = t9 * t3;
  t17 = t * t;
  t20 = u * t;
  t23 = u * u;
  t26 = t9 * q4;
  t27 = t17 * t;
  t30 = t17 * u;
  t33 = t * t23;
  t36 = t23 * u;
  t39 = t3 * t3;
  t40 = t1 * t39;
  t45 = t1 * t10;
  t52 = t1 * t3;
  t57 = 0.120e3 * t * t11 + 0.60e2 * t * t4 + 0.60e2 * t * t40 - 0.8e1 * t11 * u - 0.120e3 * t16 * t17 - 0.96e2 * t16 * t20 - 0.8e1 * t16 * t23 - 0.120e3 * t17 * t45 - 0.96e2 * t20 * t45 - 0.8e1 * t23 * t45 - 0.20e2 * t26 * t27 - 0.60e2 * t26 * t30 - 0.48e2 * t26 * t33 - 0.8e1 * t26 * t36 + 0.20e2 * t27 * t52 - 0.20e2 * t30 * t52 + 0.12e2 * t4 * u + 0.12e2 * t40 * u;
  t62 = t1 * q4;
  t63 = t17 * t17;
  t66 = t27 * u;
  t69 = t17 * t23;
  t72 = t * t36;
  t75 = t23 * t23;
  t78 = t63 * t;
  t93 = t75 * u;
  t96 = q3 * t10;
  t105 = q3 * t3;
  t110 = 0.10e2 * t * t1 * t75 + 0.20e2 * t1 * t17 * t36 + 0.20e2 * t1 * t23 * t27 + 0.10e2 * t1 * t63 * u + 0.2e1 * t1 * t78 + 0.2e1 * t1 * t93 + 0.40e2 * t105 * t63 + 0.140e3 * t105 * t66 - 0.20e2 * t27 * t96 - 0.60e2 * t30 * t96 - 0.28e2 * t33 * t52 - 0.48e2 * t33 * t96 + 0.12e2 * t36 * t52 - 0.8e1 * t36 * t96 + 0.40e2 * t62 * t63 + 0.140e3 * t62 * t66 + 0.164e3 * t62 * t69 + 0.68e2 * t62 * t72 + 0.4e1 * t62 * t75;
  t118 = q3 * q4;
  t135 = t63 * t17;
  t153 = t75 * t23;
  t161 = -0.14e2 * q3 * t * t93 - 0.40e2 * q3 * t17 * t75 - 0.50e2 * q3 * t23 * t63 - 0.60e2 * q3 * t27 * t36 - 0.22e2 * q3 * t78 * u - 0.20e2 * t * t118 * t75 - 0.64e2 * t118 * t17 * t36 - 0.92e2 * t118 * t23 * t27 - 0.62e2 * t118 * t63 * u + 0.10e2 * t3 * t63 * u - 0.4e1 * q3 * t135 - 0.2e1 * q3 * t153 + 0.164e3 * t105 * t69 + 0.68e2 * t105 * t72 + 0.4e1 * t105 * t75 - 0.16e2 * t118 * t78 - 0.2e1 * t118 * t93 + 0.2e1 * t3 * t78;
  t207 = -0.14e2 * q4 * t * t93 - 0.40e2 * q4 * t17 * t75 - 0.50e2 * q4 * t23 * t63 - 0.60e2 * q4 * t27 * t36 - 0.22e2 * q4 * t78 * u + 0.10e2 * t * t3 * t75 + 0.20e2 * t17 * t3 * t36 + 0.20e2 * t23 * t27 * t3 - 0.4e1 * q4 * t135 - 0.2e1 * q4 * t153 + 0.7e1 * t * t153 + 0.12e2 * t135 * u + 0.22e2 * t17 * t93 + 0.31e2 * t23 * t78 + 0.2e1 * t27 * t63 + 0.40e2 * t27 * t75 + 0.2e1 * t3 * t93 + 0.45e2 * t36 * t63 + t36 * t75;
  t217 = pow(0.4e1 * t118 - t17 - 0.2e1 * t20 - t23, 0.2e1);
  t224 = QQoc(z, zp, -t);
  return(0.4e1 / 0.3e1 * (t57 + t110 + t161 + t207) * Nc * CF * Nf / t / u / t217 / (z - zp) * t224);
}
double GstarGstarMeNNLOSoft::TT35 (
  double u,
  double t,
  double q3,
  double q4,
  double z,
  double zp,
  double Nc,
  double Nf,
  double CF)
{
  double t10;
  double t15;
  double t21;
  double t33;
  double t35;
  double t41;
  double t7;
  t7 = q3 * q3;
  t10 = q3 * q4;
  t15 = t * t;
  t21 = q4 * q4;
  t33 = u * u;
  t35 = -0.2e1 * q3 * t * u - 0.2e1 * q4 * t * u - 0.4e1 * q3 * t15 - 0.4e1 * q4 * t15 + 0.4e1 * t * t10 + 0.2e1 * t * t15 + 0.2e1 * t * t21 + t * t33 + 0.2e1 * t * t7 - 0.2e1 * t10 * u + 0.2e1 * t15 * u;
  t41 = QQec(z, zp, -t);
  return(-0.4e1 / 0.3e1 * Nf * (-0.3e1 * u - 0.2e1 - 0.3e1 * t + 0.3e1 * q4 + 0.3e1 * q3) * t35 * Nc * CF / t15 / u * t41);
}
double GstarGstarMeNNLOSoft::TT34 (
  double u,
  double t,
  double q3,
  double q4,
  double z,
  double zp,
  double Nc,
  double Nf,
  double CF)
{
  double t10;
  double t18;
  double t21;
  double t29;
  double t35;
  double t41;
  double t7;
  t7 = q3 * q3;
  t10 = q3 * q4;
  t18 = u * u;
  t21 = q4 * q4;
  t29 = t * t;
  t35 = -0.2e1 * q3 * t * u - 0.2e1 * q4 * t * u - 0.4e1 * q3 * t18 - 0.4e1 * q4 * t18 - 0.2e1 * t * t10 + 0.2e1 * t * t18 + 0.4e1 * t10 * u + 0.2e1 * t18 * u + 0.2e1 * t21 * u + t29 * u + 0.2e1 * t7 * u;
  t41 = QQec(z, zp, -u);
  return(-0.4e1 / 0.3e1 * Nf * (-0.3e1 * u - 0.2e1 - 0.3e1 * t + 0.3e1 * q4 + 0.3e1 * q3) * t35 * Nc * CF / t / t18 * t41);
}
double GstarGstarMeNNLOSoft::TT33 (
  double u,
  double t,
  double q3,
  double q4,
  double z,
  double zp,
  double Nc,
  double Nf,
  double CF)
{
  double t11;
  double t12;
  double t18;
  double t27;
  double t45;
  double t52;
  double t7;
  t7 = q3 * q3;
  t11 = q3 * q4;
  t12 = t * t;
  t18 = u * u;
  t27 = q4 * q4;
  t45 = -0.6e1 * q3 * t * t18 - 0.6e1 * q3 * t12 * u - 0.6e1 * q4 * t * t18 - 0.6e1 * q4 * t12 * u + 0.8e1 * t * t11 * u + 0.3e1 * t * t12 * u + 0.3e1 * t * t18 * u + 0.4e1 * t * t27 * u + 0.4e1 * t * t7 * u - 0.2e1 * t11 * t12 - 0.2e1 * t11 * t18 + 0.4e1 * t12 * t18;
  t52 = polylog(3, 0.1e1 - zp);
  return(0.8e1 / 0.3e1 * Nf * (-0.3e1 * u - 0.2e1 - 0.3e1 * t + 0.3e1 * q4 + 0.3e1 * q3) * t45 * CF * Nc / t12 / t18 * t52);
}
double GstarGstarMeNNLOSoft::TT32 (
  double u,
  double t,
  double q3,
  double q4,
  double z,
  double zp,
  double Nc,
  double Nf,
  double CF)
{
  double t11;
  double t12;
  double t18;
  double t27;
  double t45;
  double t52;
  double t7;
  t7 = q3 * q3;
  t11 = q3 * q4;
  t12 = t * t;
  t18 = u * u;
  t27 = q4 * q4;
  t45 = -0.6e1 * q3 * t * t18 - 0.6e1 * q3 * t12 * u - 0.6e1 * q4 * t * t18 - 0.6e1 * q4 * t12 * u + 0.8e1 * t * t11 * u + 0.3e1 * t * t12 * u + 0.3e1 * t * t18 * u + 0.4e1 * t * t27 * u + 0.4e1 * t * t7 * u - 0.2e1 * t11 * t12 - 0.2e1 * t11 * t18 + 0.4e1 * t12 * t18;
  t52 = polylog(3, 0.1e1 - z);
  return(0.8e1 / 0.3e1 * Nf * (-0.3e1 * u - 0.2e1 - 0.3e1 * t + 0.3e1 * q4 + 0.3e1 * q3) * t45 * CF * Nc / t12 / t18 * t52);
}
#include <math.h>

double GstarGstarMeNNLOSoft::TT31 (
  double u,
  double t,
  double q3,
  double q4,
  double z,
  double zp,
  double Nc,
  double Nf,
  double CF)
{
  double t1;
  double t10;
  double t105;
  double t11;
  double t110;
  double t118;
  double t135;
  double t153;
  double t16;
  double t161;
  double t17;
  double t2;
  double t20;
  double t207;
  double t217;
  double t221;
  double t229;
  double t23;
  double t26;
  double t27;
  double t3;
  double t30;
  double t33;
  double t36;
  double t39;
  double t4;
  double t40;
  double t45;
  double t52;
  double t57;
  double t62;
  double t63;
  double t66;
  double t69;
  double t72;
  double t75;
  double t78;
  double t9;
  double t93;
  double t96;
  t1 = q3 * q3;
  t2 = t1 * t1;
  t3 = q4 * q4;
  t4 = t2 * t3;
  t9 = t1 * q3;
  t10 = t3 * q4;
  t11 = t9 * t10;
  t16 = t9 * t3;
  t17 = t * t;
  t20 = u * t;
  t23 = u * u;
  t26 = t9 * q4;
  t27 = t17 * t;
  t30 = t17 * u;
  t33 = t * t23;
  t36 = t23 * u;
  t39 = t3 * t3;
  t40 = t1 * t39;
  t45 = t1 * t10;
  t52 = t1 * t3;
  t57 = 0.120e3 * t * t11 + 0.60e2 * t * t4 + 0.60e2 * t * t40 - 0.8e1 * t11 * u - 0.120e3 * t16 * t17 - 0.96e2 * t16 * t20 - 0.8e1 * t16 * t23 - 0.120e3 * t17 * t45 - 0.96e2 * t20 * t45 - 0.8e1 * t23 * t45 - 0.20e2 * t26 * t27 - 0.60e2 * t26 * t30 - 0.48e2 * t26 * t33 - 0.8e1 * t26 * t36 + 0.20e2 * t27 * t52 - 0.20e2 * t30 * t52 + 0.12e2 * t4 * u + 0.12e2 * t40 * u;
  t62 = t1 * q4;
  t63 = t17 * t17;
  t66 = t27 * u;
  t69 = t17 * t23;
  t72 = t * t36;
  t75 = t23 * t23;
  t78 = t63 * t;
  t93 = u * t75;
  t96 = q3 * t10;
  t105 = q3 * t3;
  t110 = 0.10e2 * t * t1 * t75 + 0.20e2 * t1 * t17 * t36 + 0.20e2 * t1 * t23 * t27 + 0.10e2 * t1 * t63 * u + 0.2e1 * t1 * t78 + 0.2e1 * t1 * t93 + 0.40e2 * t105 * t63 + 0.140e3 * t105 * t66 - 0.20e2 * t27 * t96 - 0.60e2 * t30 * t96 - 0.28e2 * t33 * t52 - 0.48e2 * t33 * t96 + 0.12e2 * t36 * t52 - 0.8e1 * t36 * t96 + 0.40e2 * t63 * t62 + 0.140e3 * t62 * t66 + 0.164e3 * t62 * t69 + 0.68e2 * t62 * t72 + 0.4e1 * t62 * t75;
  t118 = q3 * q4;
  t135 = t63 * t17;
  t153 = t23 * t75;
  t161 = -0.14e2 * q3 * t * t93 - 0.40e2 * q3 * t17 * t75 - 0.50e2 * q3 * t23 * t63 - 0.60e2 * q3 * t27 * t36 - 0.22e2 * q3 * t78 * u - 0.20e2 * t * t118 * t75 - 0.64e2 * t118 * t17 * t36 - 0.92e2 * t118 * t23 * t27 - 0.62e2 * t118 * t63 * u + 0.10e2 * t3 * t63 * u - 0.4e1 * q3 * t135 - 0.2e1 * q3 * t153 + 0.164e3 * t105 * t69 + 0.68e2 * t105 * t72 + 0.4e1 * t105 * t75 - 0.16e2 * t118 * t78 - 0.2e1 * t118 * t93 + 0.2e1 * t3 * t78;
  t207 = -0.14e2 * q4 * t * t93 - 0.40e2 * q4 * t17 * t75 - 0.50e2 * q4 * t23 * t63 - 0.60e2 * q4 * t27 * t36 - 0.22e2 * q4 * t78 * u + 0.10e2 * t * t3 * t75 + 0.20e2 * t17 * t3 * t36 + 0.20e2 * t23 * t27 * t3 - 0.4e1 * q4 * t135 - 0.2e1 * q4 * t153 + 0.7e1 * t * t153 + 0.12e2 * t135 * u + 0.22e2 * t17 * t93 + 0.31e2 * t23 * t78 + 0.2e1 * t63 * t27 + 0.40e2 * t27 * t75 + 0.2e1 * t3 * t93 + 0.45e2 * t36 * t63 + t36 * t75;
  t217 = pow(0.4e1 * t118 - t17 - 0.2e1 * t20 - t23, 0.2e1);
  t221 = z - zp;
  t229 = polylog(3, -0.1e1 / (-0.1e1 + zp) / z * t221);
  return(-0.8e1 / 0.3e1 * (t57 + t110 + t161 + t207) * Nc * CF * Nf / t / t217 / u / t221 * t229);
}
#include <math.h>

double GstarGstarMeNNLOSoft::TT30 (
  double u,
  double t,
  double q3,
  double q4,
  double z,
  double zp,
  double Nc,
  double Nf,
  double CF)
{
  double t1;
  double t10;
  double t105;
  double t11;
  double t110;
  double t118;
  double t135;
  double t153;
  double t16;
  double t161;
  double t17;
  double t2;
  double t20;
  double t207;
  double t217;
  double t221;
  double t229;
  double t23;
  double t26;
  double t27;
  double t3;
  double t30;
  double t33;
  double t36;
  double t39;
  double t4;
  double t40;
  double t45;
  double t52;
  double t57;
  double t62;
  double t63;
  double t66;
  double t69;
  double t72;
  double t75;
  double t78;
  double t9;
  double t93;
  double t96;
  t1 = q3 * q3;
  t2 = t1 * t1;
  t3 = q4 * q4;
  t4 = t2 * t3;
  t9 = t1 * q3;
  t10 = t3 * q4;
  t11 = t9 * t10;
  t16 = t9 * t3;
  t17 = t * t;
  t20 = u * t;
  t23 = u * u;
  t26 = t9 * q4;
  t27 = t17 * t;
  t30 = t17 * u;
  t33 = t * t23;
  t36 = t23 * u;
  t39 = t3 * t3;
  t40 = t1 * t39;
  t45 = t1 * t10;
  t52 = t1 * t3;
  t57 = -0.8e1 * t * t11 + 0.12e2 * t * t4 + 0.12e2 * t * t40 + 0.120e3 * t11 * u - 0.8e1 * t16 * t17 - 0.96e2 * t16 * t20 - 0.120e3 * t16 * t23 - 0.8e1 * t17 * t45 - 0.96e2 * t20 * t45 - 0.120e3 * t23 * t45 - 0.8e1 * t26 * t27 - 0.48e2 * t26 * t30 - 0.60e2 * t26 * t33 - 0.20e2 * t26 * t36 + 0.12e2 * t27 * t52 - 0.28e2 * t30 * t52 + 0.60e2 * t4 * u + 0.60e2 * t40 * u;
  t62 = t1 * q4;
  t63 = t17 * t17;
  t66 = t27 * u;
  t69 = t17 * t23;
  t72 = t * t36;
  t75 = t23 * t23;
  t78 = t63 * t;
  t93 = u * t75;
  t96 = q3 * t10;
  t105 = q3 * t3;
  t110 = 0.10e2 * t * t1 * t75 + 0.20e2 * t1 * t17 * t36 + 0.20e2 * t1 * t23 * t27 + 0.10e2 * t1 * t63 * u + 0.2e1 * t1 * t78 + 0.2e1 * t1 * t93 + 0.4e1 * t105 * t63 + 0.68e2 * t105 * t66 - 0.8e1 * t27 * t96 - 0.48e2 * t30 * t96 - 0.20e2 * t33 * t52 - 0.60e2 * t33 * t96 + 0.20e2 * t36 * t52 - 0.20e2 * t36 * t96 + 0.4e1 * t63 * t62 + 0.68e2 * t62 * t66 + 0.164e3 * t62 * t69 + 0.140e3 * t62 * t72 + 0.40e2 * t62 * t75;
  t118 = q3 * q4;
  t135 = t63 * t17;
  t153 = t23 * t75;
  t161 = -0.22e2 * q3 * t * t93 - 0.50e2 * q3 * t17 * t75 - 0.40e2 * q3 * t23 * t63 - 0.60e2 * q3 * t27 * t36 - 0.14e2 * q3 * t78 * u - 0.62e2 * t * t118 * t75 - 0.92e2 * t118 * t17 * t36 - 0.64e2 * t118 * t23 * t27 - 0.20e2 * t118 * t63 * u + 0.10e2 * t3 * t63 * u - 0.2e1 * q3 * t135 - 0.4e1 * q3 * t153 + 0.164e3 * t105 * t69 + 0.140e3 * t105 * t72 + 0.40e2 * t105 * t75 - 0.2e1 * t118 * t78 - 0.16e2 * t118 * t93 + 0.2e1 * t3 * t78;
  t207 = -0.22e2 * q4 * t * t93 - 0.50e2 * q4 * t17 * t75 - 0.40e2 * q4 * t23 * t63 - 0.60e2 * q4 * t27 * t36 - 0.14e2 * q4 * t78 * u + 0.10e2 * t * t3 * t75 + 0.20e2 * t17 * t3 * t36 + 0.20e2 * t23 * t27 * t3 - 0.2e1 * q4 * t135 - 0.4e1 * q4 * t153 + 0.12e2 * t * t153 + 0.7e1 * t135 * u + 0.31e2 * t17 * t93 + 0.22e2 * t23 * t78 + t63 * t27 + 0.45e2 * t27 * t75 + 0.2e1 * t3 * t93 + 0.40e2 * t36 * t63 + 0.2e1 * t36 * t75;
  t217 = pow(0.4e1 * t118 - t17 - 0.2e1 * t20 - t23, 0.2e1);
  t221 = z - zp;
  t229 = polylog(3, -0.1e1 / zp / (0.1e1 - z) * t221);
  return(0.8e1 / 0.3e1 * (t57 + t110 + t161 + t207) * Nc * CF * Nf / t / t217 / u / t221 * t229);
}
#include <math.h>

double GstarGstarMeNNLOSoft::TT29 (
  double u,
  double t,
  double q3,
  double q4,
  double z,
  double zp,
  double Nc,
  double Nf,
  double CF)
{
  double t1;
  double t10;
  double t105;
  double t11;
  double t110;
  double t118;
  double t135;
  double t153;
  double t16;
  double t161;
  double t17;
  double t2;
  double t20;
  double t207;
  double t217;
  double t221;
  double t227;
  double t23;
  double t26;
  double t27;
  double t3;
  double t30;
  double t33;
  double t36;
  double t39;
  double t4;
  double t40;
  double t45;
  double t52;
  double t57;
  double t62;
  double t63;
  double t66;
  double t69;
  double t72;
  double t75;
  double t78;
  double t9;
  double t93;
  double t96;
  t1 = q3 * q3;
  t2 = t1 * t1;
  t3 = q4 * q4;
  t4 = t2 * t3;
  t9 = t1 * q3;
  t10 = t3 * q4;
  t11 = t9 * t10;
  t16 = t9 * t3;
  t17 = t * t;
  t20 = u * t;
  t23 = u * u;
  t26 = t9 * q4;
  t27 = t17 * t;
  t30 = t17 * u;
  t33 = t * t23;
  t36 = t23 * u;
  t39 = t3 * t3;
  t40 = t1 * t39;
  t45 = t1 * t10;
  t52 = t1 * t3;
  t57 = 0.120e3 * t * t11 + 0.60e2 * t * t4 + 0.60e2 * t * t40 - 0.8e1 * t11 * u - 0.120e3 * t16 * t17 - 0.96e2 * t16 * t20 - 0.8e1 * t16 * t23 - 0.120e3 * t17 * t45 - 0.96e2 * t20 * t45 - 0.8e1 * t23 * t45 - 0.20e2 * t26 * t27 - 0.60e2 * t26 * t30 - 0.48e2 * t26 * t33 - 0.8e1 * t26 * t36 + 0.20e2 * t27 * t52 - 0.20e2 * t30 * t52 + 0.12e2 * t4 * u + 0.12e2 * t40 * u;
  t62 = t1 * q4;
  t63 = t17 * t17;
  t66 = t27 * u;
  t69 = t17 * t23;
  t72 = t * t36;
  t75 = t23 * t23;
  t78 = t63 * t;
  t93 = u * t75;
  t96 = q3 * t10;
  t105 = q3 * t3;
  t110 = 0.10e2 * t * t1 * t75 + 0.20e2 * t1 * t17 * t36 + 0.20e2 * t1 * t23 * t27 + 0.10e2 * t1 * t63 * u + 0.2e1 * t1 * t78 + 0.2e1 * t1 * t93 + 0.40e2 * t105 * t63 + 0.140e3 * t105 * t66 - 0.20e2 * t27 * t96 - 0.60e2 * t30 * t96 - 0.28e2 * t33 * t52 - 0.48e2 * t33 * t96 + 0.12e2 * t36 * t52 - 0.8e1 * t36 * t96 + 0.40e2 * t63 * t62 + 0.140e3 * t62 * t66 + 0.164e3 * t62 * t69 + 0.68e2 * t62 * t72 + 0.4e1 * t62 * t75;
  t118 = q3 * q4;
  t135 = t63 * t17;
  t153 = t23 * t75;
  t161 = -0.14e2 * q3 * t * t93 - 0.40e2 * q3 * t17 * t75 - 0.50e2 * q3 * t23 * t63 - 0.60e2 * q3 * t27 * t36 - 0.22e2 * q3 * t78 * u - 0.20e2 * t * t118 * t75 - 0.64e2 * t118 * t17 * t36 - 0.92e2 * t118 * t23 * t27 - 0.62e2 * t118 * t63 * u + 0.10e2 * t3 * t63 * u - 0.4e1 * q3 * t135 - 0.2e1 * q3 * t153 + 0.164e3 * t105 * t69 + 0.68e2 * t105 * t72 + 0.4e1 * t105 * t75 - 0.16e2 * t118 * t78 - 0.2e1 * t118 * t93 + 0.2e1 * t3 * t78;
  t207 = -0.14e2 * q4 * t * t93 - 0.40e2 * q4 * t17 * t75 - 0.50e2 * q4 * t23 * t63 - 0.60e2 * q4 * t27 * t36 - 0.22e2 * q4 * t78 * u + 0.10e2 * t * t3 * t75 + 0.20e2 * t17 * t3 * t36 + 0.20e2 * t23 * t27 * t3 - 0.4e1 * q4 * t135 - 0.2e1 * q4 * t153 + 0.7e1 * t * t153 + 0.12e2 * t135 * u + 0.22e2 * t17 * t93 + 0.31e2 * t23 * t78 + 0.2e1 * t63 * t27 + 0.40e2 * t27 * t75 + 0.2e1 * t3 * t93 + 0.45e2 * t36 * t63 + t36 * t75;
  t217 = pow(0.4e1 * t118 - t17 - 0.2e1 * t20 - t23, 0.2e1);
  t221 = z - zp;
  t227 = polylog(3, -t221 / (-0.1e1 + zp));
  return(0.16e2 / 0.3e1 * (t57 + t110 + t161 + t207) * Nc * CF * Nf / t / t217 / u / t221 * t227);
}

double  GstarGstarMeNNLOSoft::TT28 (
  double u,
  double t,
  double q3,
  double q4,
  double z,
  double zp,
  double Nc,
  double Nf,
  double CF)
{
  double t10;
  double t15;
  double t21;
  double t33;
  double t35;
  double t44;
  double t7;
  t7 = q3 * q3;
  t10 = q3 * q4;
  t15 = t * t;
  t21 = q4 * q4;
  t33 = u * u;
  t35 = -0.2e1 * q3 * t * u - 0.2e1 * q4 * t * u - 0.4e1 * q3 * t15 - 0.4e1 * q4 * t15 + 0.4e1 * t * t10 + 0.2e1 * t * t15 + 0.2e1 * t * t21 + t * t33 + 0.2e1 * t * t7 - 0.2e1 * t10 * u + 0.2e1 * t15 * u;
  t44 = polylog(3, q4 / (q4 - t));
  return(-0.2e1 / 0.3e1 * CF * Nc * (-0.21e2 * u - 0.13e2 - 0.21e2 * t + 0.21e2 * q4 + 0.21e2 * q3) * t35 * Nf / t15 / u * t44);
}
double  GstarGstarMeNNLOSoft::TT27 (
  double u,
  double t,
  double q3,
  double q4,
  double z,
  double zp,
  double Nc,
  double Nf,
  double CF)
{
  double t10;
  double t18;
  double t21;
  double t29;
  double t35;
  double t44;
  double t7;
  t7 = q3 * q3;
  t10 = q3 * q4;
  t18 = u * u;
  t21 = q4 * q4;
  t29 = t * t;
  t35 = -0.2e1 * q3 * t * u - 0.2e1 * q4 * t * u - 0.4e1 * q3 * t18 - 0.4e1 * q4 * t18 - 0.2e1 * t * t10 + 0.2e1 * t * t18 + 0.4e1 * t10 * u + 0.2e1 * t18 * u + 0.2e1 * t21 * u + t29 * u + 0.2e1 * t7 * u;
  t44 = polylog(3, q4 / (q4 - u));
  return(-0.2e1 / 0.3e1 * CF * Nc * (-0.21e2 * u - 0.13e2 - 0.21e2 * t + 0.21e2 * q4 + 0.21e2 * q3) * t35 * Nf / t / t18 * t44);
}
double  GstarGstarMeNNLOSoft::TT26 (
  double u,
  double t,
  double q3,
  double q4,
  double z,
  double zp,
  double Nc,
  double Nf,
  double CF)
{
  double t10;
  double t15;
  double t21;
  double t33;
  double t35;
  double t44;
  double t7;
  t7 = q3 * q3;
  t10 = q3 * q4;
  t15 = t * t;
  t21 = q4 * q4;
  t33 = u * u;
  t35 = -0.2e1 * q3 * t * u - 0.2e1 * q4 * t * u - 0.4e1 * q3 * t15 - 0.4e1 * q4 * t15 + 0.4e1 * t * t10 + 0.2e1 * t * t15 + 0.2e1 * t * t21 + t * t33 + 0.2e1 * t * t7 - 0.2e1 * t10 * u + 0.2e1 * t15 * u;
  t44 = polylog(3, -t / (q3 - t));
  return(-0.4e1 / 0.3e1 * CF * Nc * (-0.9e1 * t - 0.7e1 + 0.9e1 * q4 + 0.9e1 * q3 - 0.9e1 * u) * t35 * Nf / t15 / u * t44);
}
double  GstarGstarMeNNLOSoft::TT25 (
  double u,
  double t,
  double q3,
  double q4,
  double z,
  double zp,
  double Nc,
  double Nf,
  double CF)
{
  double t10;
  double t18;
  double t21;
  double t29;
  double t35;
  double t44;
  double t7;
  t7 = q3 * q3;
  t10 = q3 * q4;
  t18 = u * u;
  t21 = q4 * q4;
  t29 = t * t;
  t35 = -0.2e1 * q3 * t * u - 0.2e1 * q4 * t * u - 0.4e1 * q3 * t18 - 0.4e1 * q4 * t18 - 0.2e1 * t * t10 + 0.2e1 * t * t18 + 0.4e1 * t10 * u + 0.2e1 * t18 * u + 0.2e1 * t21 * u + t29 * u + 0.2e1 * t7 * u;
  t44 = polylog(3, q3 / (q3 - u));
  return(-0.2e1 / 0.3e1 * CF * Nc * (-0.21e2 * u - 0.13e2 - 0.21e2 * t + 0.21e2 * q4 + 0.21e2 * q3) * t35 * Nf / t / t18 * t44);
}
#include <math.h>

double  GstarGstarMeNNLOSoft::TT24 (
  double u,
  double t,
  double q3,
  double q4,
  double z,
  double zp,
  double Nc,
  double Nf,
  double CF)
{
  double t1;
  double t10;
  double t105;
  double t11;
  double t110;
  double t118;
  double t135;
  double t153;
  double t16;
  double t161;
  double t17;
  double t2;
  double t20;
  double t207;
  double t217;
  double t221;
  double t229;
  double t23;
  double t26;
  double t27;
  double t3;
  double t30;
  double t33;
  double t36;
  double t39;
  double t4;
  double t40;
  double t45;
  double t52;
  double t57;
  double t62;
  double t63;
  double t66;
  double t69;
  double t72;
  double t75;
  double t78;
  double t9;
  double t93;
  double t96;
  t1 = q3 * q3;
  t2 = t1 * t1;
  t3 = q4 * q4;
  t4 = t2 * t3;
  t9 = t1 * q3;
  t10 = t3 * q4;
  t11 = t9 * t10;
  t16 = t9 * t3;
  t17 = t * t;
  t20 = u * t;
  t23 = u * u;
  t26 = t9 * q4;
  t27 = t17 * t;
  t30 = t17 * u;
  t33 = t * t23;
  t36 = t23 * u;
  t39 = t3 * t3;
  t40 = t1 * t39;
  t45 = t1 * t10;
  t52 = t1 * t3;
  t57 = 0.120e3 * t * t11 + 0.60e2 * t * t4 + 0.60e2 * t * t40 - 0.8e1 * t11 * u - 0.120e3 * t16 * t17 - 0.96e2 * t16 * t20 - 0.8e1 * t16 * t23 - 0.120e3 * t17 * t45 - 0.96e2 * t20 * t45 - 0.8e1 * t23 * t45 - 0.20e2 * t26 * t27 - 0.60e2 * t26 * t30 - 0.48e2 * t26 * t33 - 0.8e1 * t26 * t36 + 0.20e2 * t27 * t52 - 0.20e2 * t30 * t52 + 0.12e2 * t4 * u + 0.12e2 * t40 * u;
  t62 = t1 * q4;
  t63 = t17 * t17;
  t66 = t27 * u;
  t69 = t17 * t23;
  t72 = t * t36;
  t75 = t23 * t23;
  t78 = t63 * t;
  t93 = t75 * u;
  t96 = q3 * t10;
  t105 = q3 * t3;
  t110 = 0.10e2 * t * t1 * t75 + 0.20e2 * t1 * t17 * t36 + 0.20e2 * t1 * t23 * t27 + 0.10e2 * t1 * t63 * u + 0.2e1 * t1 * t78 + 0.2e1 * t1 * t93 + 0.40e2 * t105 * t63 + 0.140e3 * t105 * t66 - 0.20e2 * t27 * t96 - 0.60e2 * t30 * t96 - 0.28e2 * t33 * t52 - 0.48e2 * t33 * t96 + 0.12e2 * t36 * t52 - 0.8e1 * t36 * t96 + 0.40e2 * t62 * t63 + 0.140e3 * t62 * t66 + 0.164e3 * t62 * t69 + 0.68e2 * t62 * t72 + 0.4e1 * t62 * t75;
  t118 = q3 * q4;
  t135 = t63 * t17;
  t153 = t75 * t23;
  t161 = -0.14e2 * q3 * t * t93 - 0.40e2 * q3 * t17 * t75 - 0.50e2 * q3 * t23 * t63 - 0.60e2 * q3 * t27 * t36 - 0.22e2 * q3 * t78 * u - 0.20e2 * t * t118 * t75 - 0.64e2 * t118 * t17 * t36 - 0.92e2 * t118 * t23 * t27 - 0.62e2 * t118 * t63 * u + 0.10e2 * t3 * t63 * u - 0.4e1 * q3 * t135 - 0.2e1 * q3 * t153 + 0.164e3 * t105 * t69 + 0.68e2 * t105 * t72 + 0.4e1 * t105 * t75 - 0.16e2 * t118 * t78 - 0.2e1 * t118 * t93 + 0.2e1 * t3 * t78;
  t207 = -0.14e2 * q4 * t * t93 - 0.40e2 * q4 * t17 * t75 - 0.50e2 * q4 * t23 * t63 - 0.60e2 * q4 * t27 * t36 - 0.22e2 * q4 * t78 * u + 0.10e2 * t * t3 * t75 + 0.20e2 * t17 * t3 * t36 + 0.20e2 * t23 * t27 * t3 - 0.4e1 * q4 * t135 - 0.2e1 * q4 * t153 + 0.7e1 * t * t153 + 0.12e2 * t135 * u + 0.22e2 * t17 * t93 + 0.31e2 * t78 * t23 + 0.2e1 * t27 * t63 + 0.40e2 * t27 * t75 + 0.2e1 * t3 * t93 + 0.45e2 * t36 * t63 + t36 * t75;
  t217 = pow(0.4e1 * t118 - t17 - 0.2e1 * t20 - t23, 0.2e1);
  t221 = z - zp;
  t229 = polylog(3, 0.1e1 / (z - 0.1e1) / zp * t221);
  return(0.8e1 / 0.3e1 * (t57 + t110 + t161 + t207) * Nc * CF * Nf / t / t217 / u / t221 * t229);
}
#include <math.h>

double  GstarGstarMeNNLOSoft::TT23 (
  double u,
  double t,
  double q3,
  double q4,
  double z,
  double zp,
  double Nc,
  double Nf,
  double CF)
{
  double t10;
  double t106;
  double t11;
  double t111;
  double t119;
  double t12;
  double t136;
  double t154;
  double t162;
  double t17;
  double t18;
  double t2;
  double t208;
  double t21;
  double t217;
  double t221;
  double t229;
  double t24;
  double t27;
  double t28;
  double t3;
  double t31;
  double t34;
  double t37;
  double t4;
  double t40;
  double t41;
  double t46;
  double t5;
  double t53;
  double t58;
  double t63;
  double t64;
  double t67;
  double t70;
  double t73;
  double t76;
  double t79;
  double t94;
  double t97;
  t2 = q3 * q3;
  t3 = t2 * t2;
  t4 = q4 * q4;
  t5 = t3 * t4;
  t10 = t2 * q3;
  t11 = t4 * q4;
  t12 = t10 * t11;
  t17 = t10 * t4;
  t18 = t * t;
  t21 = u * t;
  t24 = u * u;
  t27 = t10 * q4;
  t28 = t18 * t;
  t31 = t18 * u;
  t34 = t * t24;
  t37 = t24 * u;
  t40 = t4 * t4;
  t41 = t2 * t40;
  t46 = t2 * t11;
  t53 = t2 * t4;
  t58 = -0.8e1 * t * t12 + 0.12e2 * t * t41 + 0.12e2 * t * t5 + 0.120e3 * t12 * u - 0.8e1 * t17 * t18 - 0.96e2 * t17 * t21 - 0.120e3 * t17 * t24 - 0.8e1 * t18 * t46 - 0.96e2 * t21 * t46 - 0.120e3 * t24 * t46 - 0.8e1 * t27 * t28 - 0.48e2 * t27 * t31 - 0.60e2 * t27 * t34 - 0.20e2 * t27 * t37 + 0.12e2 * t28 * t53 - 0.28e2 * t31 * t53 + 0.60e2 * t41 * u + 0.60e2 * t5 * u;
  t63 = t2 * q4;
  t64 = t18 * t18;
  t67 = t28 * u;
  t70 = t18 * t24;
  t73 = t * t37;
  t76 = t24 * t24;
  t79 = t64 * t;
  t94 = t76 * u;
  t97 = q3 * t11;
  t106 = q3 * t4;
  t111 = 0.10e2 * t * t2 * t76 + 0.20e2 * t18 * t2 * t37 + 0.20e2 * t2 * t24 * t28 + 0.10e2 * t2 * t64 * u + 0.4e1 * t106 * t64 + 0.68e2 * t106 * t67 + 0.2e1 * t2 * t79 + 0.2e1 * t2 * t94 - 0.8e1 * t28 * t97 - 0.48e2 * t31 * t97 - 0.20e2 * t34 * t53 - 0.60e2 * t34 * t97 + 0.20e2 * t37 * t53 - 0.20e2 * t37 * t97 + 0.4e1 * t63 * t64 + 0.68e2 * t63 * t67 + 0.164e3 * t63 * t70 + 0.140e3 * t63 * t73 + 0.40e2 * t63 * t76;
  t119 = q3 * q4;
  t136 = t64 * t18;
  t154 = t76 * t24;
  t162 = -0.22e2 * q3 * t * t94 - 0.50e2 * q3 * t18 * t76 - 0.40e2 * q3 * t24 * t64 - 0.60e2 * q3 * t28 * t37 - 0.14e2 * q3 * t79 * u - 0.62e2 * t * t119 * t76 - 0.92e2 * t119 * t18 * t37 - 0.64e2 * t119 * t24 * t28 - 0.20e2 * t119 * t64 * u + 0.10e2 * t4 * t64 * u - 0.2e1 * q3 * t136 - 0.4e1 * q3 * t154 + 0.164e3 * t106 * t70 + 0.140e3 * t106 * t73 + 0.40e2 * t76 * t106 - 0.2e1 * t119 * t79 - 0.16e2 * t119 * t94 + 0.2e1 * t4 * t79;
  t208 = -0.22e2 * q4 * t * t94 - 0.50e2 * q4 * t18 * t76 - 0.40e2 * q4 * t24 * t64 - 0.60e2 * q4 * t28 * t37 - 0.14e2 * q4 * t79 * u + 0.10e2 * t * t4 * t76 + 0.20e2 * t18 * t37 * t4 + 0.20e2 * t24 * t28 * t4 - 0.2e1 * q4 * t136 - 0.4e1 * q4 * t154 + 0.12e2 * t * t154 + 0.7e1 * t136 * u + 0.31e2 * t18 * t94 + 0.22e2 * t24 * t79 + t28 * t64 + 0.45e2 * t76 * t28 + 0.40e2 * t37 * t64 + 0.2e1 * t37 * t76 + 0.2e1 * t4 * t94;
  t217 = pow(0.4e1 * t119 - t18 - 0.2e1 * t21 - t24, 0.2e1);
  t221 = z - zp;
  t229 = polylog(3, -0.1e1 / (-0.1e1 + zp) / z * t221);
  return(-0.8e1 / 0.3e1 * Nc * CF * (t58 + t111 + t162 + t208) * Nf / t / t217 / u / t221 * t229);
}
#include <math.h>

double  GstarGstarMeNNLOSoft::TT22 (
  double u,
  double t,
  double q3,
  double q4,
  double z,
  double zp,
  double Nc,
  double Nf,
  double CF)
{
  double t10;
  double t106;
  double t11;
  double t111;
  double t119;
  double t12;
  double t136;
  double t154;
  double t162;
  double t17;
  double t18;
  double t2;
  double t209;
  double t21;
  double t218;
  double t222;
  double t227;
  double t24;
  double t27;
  double t28;
  double t3;
  double t31;
  double t34;
  double t37;
  double t4;
  double t40;
  double t41;
  double t46;
  double t5;
  double t53;
  double t58;
  double t63;
  double t64;
  double t67;
  double t70;
  double t73;
  double t76;
  double t79;
  double t94;
  double t97;
  t2 = q3 * q3;
  t3 = t2 * t2;
  t4 = q4 * q4;
  t5 = t3 * t4;
  t10 = t2 * q3;
  t11 = t4 * q4;
  t12 = t10 * t11;
  t17 = t10 * t4;
  t18 = t * t;
  t21 = u * t;
  t24 = u * u;
  t27 = t10 * q4;
  t28 = t18 * t;
  t31 = t18 * u;
  t34 = t * t24;
  t37 = t24 * u;
  t40 = t4 * t4;
  t41 = t2 * t40;
  t46 = t2 * t11;
  t53 = t2 * t4;
  t58 = 0.112e3 * t * t12 + 0.72e2 * t * t41 + 0.72e2 * t * t5 + 0.112e3 * t12 * u - 0.128e3 * t17 * t18 - 0.192e3 * t17 * t21 - 0.128e3 * t17 * t24 - 0.128e3 * t18 * t46 - 0.192e3 * t21 * t46 - 0.128e3 * t24 * t46 - 0.28e2 * t27 * t28 - 0.108e3 * t27 * t31 - 0.108e3 * t27 * t34 - 0.28e2 * t27 * t37 + 0.32e2 * t28 * t53 - 0.48e2 * t31 * t53 + 0.72e2 * t41 * u + 0.72e2 * t5 * u;
  t63 = t2 * q4;
  t64 = t18 * t18;
  t67 = t28 * u;
  t70 = t18 * t24;
  t73 = t * t37;
  t76 = t24 * t24;
  t79 = t64 * t;
  t94 = t76 * u;
  t97 = q3 * t11;
  t106 = q3 * t4;
  t111 = 0.20e2 * t * t2 * t76 + 0.40e2 * t18 * t2 * t37 + 0.40e2 * t2 * t24 * t28 + 0.20e2 * t2 * t64 * u + 0.44e2 * t106 * t64 + 0.208e3 * t106 * t67 + 0.4e1 * t2 * t79 + 0.4e1 * t2 * t94 - 0.28e2 * t28 * t97 - 0.108e3 * t31 * t97 - 0.48e2 * t34 * t53 - 0.108e3 * t34 * t97 + 0.32e2 * t37 * t53 - 0.28e2 * t37 * t97 + 0.44e2 * t63 * t64 + 0.208e3 * t63 * t67 + 0.328e3 * t63 * t70 + 0.208e3 * t63 * t73 + 0.44e2 * t63 * t76;
  t119 = q3 * q4;
  t136 = t64 * t18;
  t154 = t76 * t24;
  t162 = -0.36e2 * q3 * t * t94 - 0.90e2 * q3 * t18 * t76 - 0.90e2 * q3 * t24 * t64 - 0.120e3 * q3 * t28 * t37 - 0.36e2 * q3 * t79 * u - 0.82e2 * t * t119 * t76 - 0.156e3 * t119 * t18 * t37 - 0.156e3 * t119 * t24 * t28 - 0.82e2 * t119 * t64 * u + 0.20e2 * t4 * t64 * u - 0.6e1 * q3 * t136 - 0.6e1 * q3 * t154 + 0.328e3 * t106 * t70 + 0.208e3 * t106 * t73 + 0.44e2 * t76 * t106 - 0.18e2 * t119 * t79 - 0.18e2 * t119 * t94 + 0.4e1 * t4 * t79;
  t209 = -0.36e2 * q4 * t * t94 - 0.90e2 * q4 * t18 * t76 - 0.90e2 * q4 * t24 * t64 - 0.120e3 * q4 * t28 * t37 - 0.36e2 * q4 * t79 * u + 0.20e2 * t * t4 * t76 + 0.40e2 * t18 * t37 * t4 + 0.40e2 * t24 * t28 * t4 - 0.6e1 * q4 * t136 - 0.6e1 * q4 * t154 + 0.19e2 * t * t154 + 0.19e2 * t136 * u + 0.53e2 * t18 * t94 + 0.53e2 * t24 * t79 + 0.3e1 * t28 * t64 + 0.85e2 * t76 * t28 + 0.85e2 * t37 * t64 + 0.3e1 * t37 * t76 + 0.4e1 * t4 * t94;
  t218 = pow(0.4e1 * t119 - t18 - 0.2e1 * t21 - t24, 0.2e1);
  t222 = z - zp;
  t227 = polylog(3, t222 / z);
  return(0.16e2 / 0.3e1 * Nc * CF * (t58 + t111 + t162 + t209) * Nf / t / t218 / u / t222 * t227);
}
#include <math.h>

double  GstarGstarMeNNLOSoft::TT21 (
  double u,
  double t,
  double q3,
  double q4,
  double z,
  double zp,
  double Nc,
  double Nf,
  double CF)
{
  double t1;
  double t10;
  double t105;
  double t11;
  double t110;
  double t118;
  double t135;
  double t153;
  double t16;
  double t161;
  double t17;
  double t2;
  double t20;
  double t207;
  double t217;
  double t228;
  double t23;
  double t26;
  double t27;
  double t3;
  double t30;
  double t33;
  double t36;
  double t39;
  double t4;
  double t40;
  double t45;
  double t52;
  double t57;
  double t62;
  double t63;
  double t66;
  double t69;
  double t72;
  double t75;
  double t78;
  double t9;
  double t93;
  double t96;
  t1 = q3 * q3;
  t2 = t1 * t1;
  t3 = q4 * q4;
  t4 = t2 * t3;
  t9 = t1 * q3;
  t10 = t3 * q4;
  t11 = t9 * t10;
  t16 = t9 * t3;
  t17 = t * t;
  t20 = u * t;
  t23 = u * u;
  t26 = t9 * q4;
  t27 = t17 * t;
  t30 = t17 * u;
  t33 = t * t23;
  t36 = t23 * u;
  t39 = t3 * t3;
  t40 = t1 * t39;
  t45 = t1 * t10;
  t52 = t1 * t3;
  t57 = 0.120e3 * t * t11 + 0.60e2 * t * t4 + 0.60e2 * t * t40 - 0.8e1 * t11 * u - 0.120e3 * t16 * t17 - 0.96e2 * t16 * t20 - 0.8e1 * t16 * t23 - 0.120e3 * t17 * t45 - 0.96e2 * t20 * t45 - 0.8e1 * t23 * t45 - 0.20e2 * t26 * t27 - 0.60e2 * t26 * t30 - 0.48e2 * t26 * t33 - 0.8e1 * t26 * t36 + 0.20e2 * t27 * t52 - 0.20e2 * t30 * t52 + 0.12e2 * t4 * u + 0.12e2 * t40 * u;
  t62 = t1 * q4;
  t63 = t17 * t17;
  t66 = t27 * u;
  t69 = t17 * t23;
  t72 = t * t36;
  t75 = t23 * t23;
  t78 = t63 * t;
  t93 = t75 * u;
  t96 = q3 * t10;
  t105 = q3 * t3;
  t110 = 0.10e2 * t * t1 * t75 + 0.20e2 * t1 * t17 * t36 + 0.20e2 * t1 * t23 * t27 + 0.10e2 * t1 * t63 * u + 0.2e1 * t1 * t78 + 0.2e1 * t1 * t93 + 0.40e2 * t105 * t63 + 0.140e3 * t105 * t66 - 0.20e2 * t27 * t96 - 0.60e2 * t30 * t96 - 0.28e2 * t33 * t52 - 0.48e2 * t33 * t96 + 0.12e2 * t36 * t52 - 0.8e1 * t36 * t96 + 0.40e2 * t62 * t63 + 0.140e3 * t62 * t66 + 0.164e3 * t62 * t69 + 0.68e2 * t62 * t72 + 0.4e1 * t62 * t75;
  t118 = q3 * q4;
  t135 = t63 * t17;
  t153 = t75 * t23;
  t161 = -0.14e2 * q3 * t * t93 - 0.40e2 * q3 * t17 * t75 - 0.50e2 * q3 * t23 * t63 - 0.60e2 * q3 * t27 * t36 - 0.22e2 * q3 * t78 * u - 0.20e2 * t * t118 * t75 - 0.64e2 * t118 * t17 * t36 - 0.92e2 * t118 * t23 * t27 - 0.62e2 * t118 * t63 * u + 0.10e2 * t3 * t63 * u - 0.4e1 * q3 * t135 - 0.2e1 * q3 * t153 + 0.164e3 * t105 * t69 + 0.68e2 * t105 * t72 + 0.4e1 * t105 * t75 - 0.16e2 * t118 * t78 - 0.2e1 * t118 * t93 + 0.2e1 * t3 * t78;
  t207 = -0.14e2 * q4 * t * t93 - 0.40e2 * q4 * t17 * t75 - 0.50e2 * q4 * t23 * t63 - 0.60e2 * q4 * t27 * t36 - 0.22e2 * q4 * t78 * u + 0.10e2 * t * t3 * t75 + 0.20e2 * t17 * t3 * t36 + 0.20e2 * t23 * t27 * t3 - 0.4e1 * q4 * t135 - 0.2e1 * q4 * t153 + 0.7e1 * t * t153 + 0.12e2 * t135 * u + 0.22e2 * t17 * t93 + 0.31e2 * t23 * t78 + 0.2e1 * t27 * t63 + 0.40e2 * t27 * t75 + 0.2e1 * t3 * t93 + 0.45e2 * t36 * t63 + t36 * t75;
  t217 = pow(0.4e1 * t118 - t17 - 0.2e1 * t20 - t23, 0.2e1);
  t228 = polylog(3, (z - 0.1e1) / (-0.1e1 + zp));
  return(0.8e1 / 0.3e1 * (t57 + t110 + t161 + t207) * Nc * CF * Nf / t / t217 / u / (z - zp) * t228);
}
#include <math.h>

double  GstarGstarMeNNLOSoft::TT20 (
  double u,
  double t,
  double q3,
  double q4,
  double z,
  double zp,
  double Nc,
  double Nf,
  double CF)
{
  double t10;
  double t106;
  double t11;
  double t111;
  double t119;
  double t12;
  double t136;
  double t154;
  double t162;
  double t17;
  double t18;
  double t2;
  double t208;
  double t21;
  double t217;
  double t221;
  double t227;
  double t24;
  double t27;
  double t28;
  double t3;
  double t31;
  double t34;
  double t37;
  double t4;
  double t40;
  double t41;
  double t46;
  double t5;
  double t53;
  double t58;
  double t63;
  double t64;
  double t67;
  double t70;
  double t73;
  double t76;
  double t79;
  double t94;
  double t97;
  t2 = q3 * q3;
  t3 = t2 * t2;
  t4 = q4 * q4;
  t5 = t3 * t4;
  t10 = t2 * q3;
  t11 = t4 * q4;
  t12 = t10 * t11;
  t17 = t10 * t4;
  t18 = t * t;
  t21 = u * t;
  t24 = u * u;
  t27 = t10 * q4;
  t28 = t18 * t;
  t31 = t18 * u;
  t34 = t * t24;
  t37 = t24 * u;
  t40 = t4 * t4;
  t41 = t2 * t40;
  t46 = t2 * t11;
  t53 = t2 * t4;
  t58 = -0.8e1 * t * t12 + 0.12e2 * t * t41 + 0.12e2 * t * t5 + 0.120e3 * t12 * u - 0.8e1 * t17 * t18 - 0.96e2 * t17 * t21 - 0.120e3 * t17 * t24 - 0.8e1 * t18 * t46 - 0.96e2 * t21 * t46 - 0.120e3 * t24 * t46 - 0.8e1 * t27 * t28 - 0.48e2 * t27 * t31 - 0.60e2 * t27 * t34 - 0.20e2 * t27 * t37 + 0.12e2 * t28 * t53 - 0.28e2 * t31 * t53 + 0.60e2 * t41 * u + 0.60e2 * t5 * u;
  t63 = t2 * q4;
  t64 = t18 * t18;
  t67 = t28 * u;
  t70 = t18 * t24;
  t73 = t * t37;
  t76 = t24 * t24;
  t79 = t64 * t;
  t94 = t76 * u;
  t97 = q3 * t11;
  t106 = q3 * t4;
  t111 = 0.10e2 * t * t2 * t76 + 0.20e2 * t18 * t2 * t37 + 0.20e2 * t2 * t24 * t28 + 0.10e2 * t2 * t64 * u + 0.4e1 * t106 * t64 + 0.68e2 * t106 * t67 + 0.2e1 * t2 * t79 + 0.2e1 * t2 * t94 - 0.8e1 * t28 * t97 - 0.48e2 * t31 * t97 - 0.20e2 * t34 * t53 - 0.60e2 * t34 * t97 + 0.20e2 * t37 * t53 - 0.20e2 * t37 * t97 + 0.4e1 * t63 * t64 + 0.68e2 * t63 * t67 + 0.164e3 * t63 * t70 + 0.140e3 * t63 * t73 + 0.40e2 * t63 * t76;
  t119 = q3 * q4;
  t136 = t64 * t18;
  t154 = t76 * t24;
  t162 = -0.22e2 * q3 * t * t94 - 0.50e2 * q3 * t18 * t76 - 0.40e2 * q3 * t24 * t64 - 0.60e2 * q3 * t28 * t37 - 0.14e2 * q3 * t79 * u - 0.62e2 * t * t119 * t76 - 0.92e2 * t119 * t18 * t37 - 0.64e2 * t119 * t24 * t28 - 0.20e2 * t119 * t64 * u + 0.10e2 * t4 * t64 * u - 0.2e1 * q3 * t136 - 0.4e1 * q3 * t154 + 0.164e3 * t106 * t70 + 0.140e3 * t106 * t73 + 0.40e2 * t76 * t106 - 0.2e1 * t119 * t79 - 0.16e2 * t119 * t94 + 0.2e1 * t4 * t79;
  t208 = -0.22e2 * q4 * t * t94 - 0.50e2 * q4 * t18 * t76 - 0.40e2 * q4 * t24 * t64 - 0.60e2 * q4 * t28 * t37 - 0.14e2 * q4 * t79 * u + 0.10e2 * t * t4 * t76 + 0.20e2 * t18 * t37 * t4 + 0.20e2 * t24 * t28 * t4 - 0.2e1 * q4 * t136 - 0.4e1 * q4 * t154 + 0.12e2 * t * t154 + 0.7e1 * t136 * u + 0.31e2 * t18 * t94 + 0.22e2 * t24 * t79 + t28 * t64 + 0.45e2 * t76 * t28 + 0.40e2 * t37 * t64 + 0.2e1 * t37 * t76 + 0.2e1 * t4 * t94;
  t217 = pow(0.4e1 * t119 - t18 - 0.2e1 * t21 - t24, 0.2e1);
  t221 = z - zp;
  t227 = polylog(3, -t221 / (-0.1e1 + zp));
  return(0.16e2 / 0.3e1 * Nc * CF * (t58 + t111 + t162 + t208) * Nf / t / t217 / u / t221 * t227);
}
#include <math.h>

double  GstarGstarMeNNLOSoft::TT19 (
  double u,
  double t,
  double q3,
  double q4,
  double z,
  double zp,
  double Nc,
  double Nf,
  double CF)
{
  double t10;
  double t106;
  double t11;
  double t111;
  double t119;
  double t12;
  double t136;
  double t154;
  double t162;
  double t17;
  double t18;
  double t2;
  double t208;
  double t21;
  double t217;
  double t228;
  double t24;
  double t27;
  double t28;
  double t3;
  double t31;
  double t34;
  double t37;
  double t4;
  double t40;
  double t41;
  double t46;
  double t5;
  double t53;
  double t58;
  double t63;
  double t64;
  double t67;
  double t70;
  double t73;
  double t76;
  double t79;
  double t94;
  double t97;
  t2 = q3 * q3;
  t3 = t2 * t2;
  t4 = q4 * q4;
  t5 = t3 * t4;
  t10 = t2 * q3;
  t11 = t4 * q4;
  t12 = t10 * t11;
  t17 = t10 * t4;
  t18 = t * t;
  t21 = u * t;
  t24 = u * u;
  t27 = t10 * q4;
  t28 = t18 * t;
  t31 = t18 * u;
  t34 = t * t24;
  t37 = t24 * u;
  t40 = t4 * t4;
  t41 = t2 * t40;
  t46 = t2 * t11;
  t53 = t2 * t4;
  t58 = -0.8e1 * t * t12 + 0.12e2 * t * t41 + 0.12e2 * t * t5 + 0.120e3 * t12 * u - 0.8e1 * t17 * t18 - 0.96e2 * t17 * t21 - 0.120e3 * t17 * t24 - 0.8e1 * t18 * t46 - 0.96e2 * t21 * t46 - 0.120e3 * t24 * t46 - 0.8e1 * t27 * t28 - 0.48e2 * t27 * t31 - 0.60e2 * t27 * t34 - 0.20e2 * t27 * t37 + 0.12e2 * t28 * t53 - 0.28e2 * t31 * t53 + 0.60e2 * t41 * u + 0.60e2 * t5 * u;
  t63 = t2 * q4;
  t64 = t18 * t18;
  t67 = t28 * u;
  t70 = t18 * t24;
  t73 = t * t37;
  t76 = t24 * t24;
  t79 = t64 * t;
  t94 = t76 * u;
  t97 = q3 * t11;
  t106 = q3 * t4;
  t111 = 0.10e2 * t * t2 * t76 + 0.20e2 * t18 * t2 * t37 + 0.20e2 * t2 * t24 * t28 + 0.10e2 * t2 * t64 * u + 0.4e1 * t106 * t64 + 0.68e2 * t106 * t67 + 0.2e1 * t2 * t79 + 0.2e1 * t2 * t94 - 0.8e1 * t28 * t97 - 0.48e2 * t31 * t97 - 0.20e2 * t34 * t53 - 0.60e2 * t34 * t97 + 0.20e2 * t37 * t53 - 0.20e2 * t37 * t97 + 0.4e1 * t63 * t64 + 0.68e2 * t63 * t67 + 0.164e3 * t63 * t70 + 0.140e3 * t63 * t73 + 0.40e2 * t63 * t76;
  t119 = q3 * q4;
  t136 = t64 * t18;
  t154 = t24 * t76;
  t162 = -0.22e2 * q3 * t * t94 - 0.50e2 * q3 * t18 * t76 - 0.40e2 * q3 * t24 * t64 - 0.60e2 * q3 * t28 * t37 - 0.14e2 * q3 * t79 * u - 0.62e2 * t * t119 * t76 - 0.92e2 * t119 * t18 * t37 - 0.64e2 * t119 * t24 * t28 - 0.20e2 * t119 * t64 * u + 0.10e2 * t4 * t64 * u - 0.2e1 * q3 * t136 - 0.4e1 * q3 * t154 + 0.164e3 * t106 * t70 + 0.140e3 * t106 * t73 + 0.40e2 * t106 * t76 - 0.2e1 * t119 * t79 - 0.16e2 * t119 * t94 + 0.2e1 * t4 * t79;
  t208 = -0.22e2 * q4 * t * t94 - 0.50e2 * q4 * t18 * t76 - 0.40e2 * q4 * t24 * t64 - 0.60e2 * q4 * t28 * t37 - 0.14e2 * q4 * t79 * u + 0.10e2 * t * t4 * t76 + 0.20e2 * t18 * t37 * t4 + 0.20e2 * t24 * t28 * t4 - 0.2e1 * q4 * t136 - 0.4e1 * q4 * t154 + 0.12e2 * t * t154 + 0.7e1 * t136 * u + 0.31e2 * t18 * t94 + 0.22e2 * t24 * t79 + t28 * t64 + 0.45e2 * t28 * t76 + 0.40e2 * t37 * t64 + 0.2e1 * t37 * t76 + 0.2e1 * t4 * t94;
  t217 = pow(0.4e1 * t119 - t18 - 0.2e1 * t21 - t24, 0.2e1);
  t228 = polylog(3, (z - 0.1e1) / (-0.1e1 + zp));
  return(0.8e1 / 0.3e1 * Nc * CF * (t58 + t111 + t162 + t208) * Nf / t / t217 / u / (z - zp) * t228);
}
#include <math.h>

double  GstarGstarMeNNLOSoft::TT18 (
  double u,
  double t,
  double q3,
  double q4,
  double z,
  double zp,
  double Nc,
  double Nf,
  double CF)
{
  double t10;
  double t106;
  double t11;
  double t111;
  double t119;
  double t12;
  double t136;
  double t154;
  double t162;
  double t17;
  double t18;
  double t2;
  double t209;
  double t21;
  double t218;
  double t227;
  double t24;
  double t27;
  double t28;
  double t3;
  double t31;
  double t34;
  double t37;
  double t4;
  double t40;
  double t41;
  double t46;
  double t5;
  double t53;
  double t58;
  double t63;
  double t64;
  double t67;
  double t70;
  double t73;
  double t76;
  double t79;
  double t94;
  double t97;
  t2 = q3 * q3;
  t3 = t2 * t2;
  t4 = q4 * q4;
  t5 = t3 * t4;
  t10 = t2 * q3;
  t11 = t4 * q4;
  t12 = t10 * t11;
  t17 = t10 * t4;
  t18 = t * t;
  t21 = u * t;
  t24 = u * u;
  t27 = t10 * q4;
  t28 = t18 * t;
  t31 = t18 * u;
  t34 = t * t24;
  t37 = t24 * u;
  t40 = t4 * t4;
  t41 = t2 * t40;
  t46 = t2 * t11;
  t53 = t2 * t4;
  t58 = 0.112e3 * t * t12 + 0.72e2 * t * t41 + 0.72e2 * t * t5 + 0.112e3 * t12 * u - 0.128e3 * t17 * t18 - 0.192e3 * t17 * t21 - 0.128e3 * t17 * t24 - 0.128e3 * t18 * t46 - 0.192e3 * t21 * t46 - 0.128e3 * t24 * t46 - 0.28e2 * t27 * t28 - 0.108e3 * t27 * t31 - 0.108e3 * t27 * t34 - 0.28e2 * t27 * t37 + 0.32e2 * t28 * t53 - 0.48e2 * t31 * t53 + 0.72e2 * t41 * u + 0.72e2 * t5 * u;
  t63 = t2 * q4;
  t64 = t18 * t18;
  t67 = t28 * u;
  t70 = t18 * t24;
  t73 = t * t37;
  t76 = t24 * t24;
  t79 = t64 * t;
  t94 = t76 * u;
  t97 = q3 * t11;
  t106 = q3 * t4;
  t111 = 0.20e2 * t * t2 * t76 + 0.40e2 * t18 * t2 * t37 + 0.40e2 * t2 * t24 * t28 + 0.20e2 * t2 * t64 * u + 0.44e2 * t106 * t64 + 0.208e3 * t106 * t67 + 0.4e1 * t2 * t79 + 0.4e1 * t2 * t94 - 0.28e2 * t28 * t97 - 0.108e3 * t31 * t97 - 0.48e2 * t34 * t53 - 0.108e3 * t34 * t97 + 0.32e2 * t37 * t53 - 0.28e2 * t37 * t97 + 0.44e2 * t63 * t64 + 0.208e3 * t63 * t67 + 0.328e3 * t63 * t70 + 0.208e3 * t63 * t73 + 0.44e2 * t63 * t76;
  t119 = q3 * q4;
  t136 = t64 * t18;
  t154 = t76 * t24;
  t162 = -0.36e2 * q3 * t * t94 - 0.90e2 * q3 * t18 * t76 - 0.90e2 * q3 * t24 * t64 - 0.120e3 * q3 * t28 * t37 - 0.36e2 * q3 * t79 * u - 0.82e2 * t * t119 * t76 - 0.156e3 * t119 * t18 * t37 - 0.156e3 * t119 * t24 * t28 - 0.82e2 * t119 * t64 * u + 0.20e2 * t4 * t64 * u - 0.6e1 * q3 * t136 - 0.6e1 * q3 * t154 + 0.328e3 * t106 * t70 + 0.208e3 * t106 * t73 + 0.44e2 * t106 * t76 - 0.18e2 * t119 * t79 - 0.18e2 * t119 * t94 + 0.4e1 * t4 * t79;
  t209 = -0.36e2 * q4 * t * t94 - 0.90e2 * q4 * t18 * t76 - 0.90e2 * q4 * t24 * t64 - 0.120e3 * q4 * t28 * t37 - 0.36e2 * q4 * t79 * u + 0.20e2 * t * t4 * t76 + 0.40e2 * t18 * t37 * t4 + 0.40e2 * t24 * t28 * t4 - 0.6e1 * q4 * t136 - 0.6e1 * q4 * t154 + 0.19e2 * t * t154 + 0.19e2 * t136 * u + 0.53e2 * t18 * t94 + 0.53e2 * t24 * t79 + 0.3e1 * t28 * t64 + 0.85e2 * t28 * t76 + 0.85e2 * t37 * t64 + 0.3e1 * t37 * t76 + 0.4e1 * t4 * t94;
  t218 = pow(0.4e1 * t119 - t18 - 0.2e1 * t21 - t24, 0.2e1);
  t227 = polylog(3, zp / z);
  return(0.8e1 / 0.3e1 * CF * Nc * (t58 + t111 + t162 + t209) * Nf / t / t218 / u / (z - zp) * t227);
}
double  GstarGstarMeNNLOSoft::TT17 (
  double u,
  double t,
  double q3,
  double q4,
  double z,
  double zp,
  double Nc,
  double Nf,
  double CF)
{
  double t10;
  double t15;
  double t21;
  double t33;
  double t35;
  double t44;
  double t7;
  t7 = q3 * q3;
  t10 = q3 * q4;
  t15 = t * t;
  t21 = q4 * q4;
  t33 = u * u;
  t35 = -0.2e1 * q3 * t * u - 0.2e1 * q4 * t * u - 0.4e1 * q3 * t15 - 0.4e1 * q4 * t15 + 0.4e1 * t * t10 + 0.2e1 * t * t15 + 0.2e1 * t * t21 + t * t33 + 0.2e1 * t * t7 - 0.2e1 * t10 * u + 0.2e1 * t15 * u;
  t44 = polylog(3, -t / (q4 - t));
  return(-0.4e1 / 0.3e1 * CF * Nc * (-0.9e1 * t - 0.7e1 + 0.9e1 * q4 + 0.9e1 * q3 - 0.9e1 * u) * t35 * Nf / t15 / u * t44);
}
double  GstarGstarMeNNLOSoft::TT16 (
  double u,
  double t,
  double q3,
  double q4,
  double z,
  double zp,
  double Nc,
  double Nf,
  double CF)
{
  double t10;
  double t15;
  double t21;
  double t33;
  double t35;
  double t44;
  double t7;
  t7 = q3 * q3;
  t10 = q3 * q4;
  t15 = t * t;
  t21 = q4 * q4;
  t33 = u * u;
  t35 = -0.2e1 * q3 * t * u - 0.2e1 * q4 * t * u - 0.4e1 * q3 * t15 - 0.4e1 * q4 * t15 + 0.4e1 * t * t10 + 0.2e1 * t * t15 + 0.2e1 * t * t21 + t * t33 + 0.2e1 * t * t7 - 0.2e1 * t10 * u + 0.2e1 * t15 * u;
  t44 = polylog(3, q3 / (q3 - t));
  return(-0.2e1 / 0.3e1 * CF * Nc * (-0.21e2 * u - 0.13e2 - 0.21e2 * t + 0.21e2 * q4 + 0.21e2 * q3) * t35 * Nf / t15 / u * t44);
}
double  GstarGstarMeNNLOSoft::TT15 (
  double u,
  double t,
  double q3,
  double q4,
  double z,
  double zp,
  double Nc,
  double Nf,
  double CF)
{
  double t10;
  double t18;
  double t21;
  double t29;
  double t35;
  double t44;
  double t7;
  t7 = q3 * q3;
  t10 = q3 * q4;
  t18 = u * u;
  t21 = q4 * q4;
  t29 = t * t;
  t35 = -0.2e1 * q3 * t * u - 0.2e1 * q4 * t * u - 0.4e1 * q3 * t18 - 0.4e1 * q4 * t18 - 0.2e1 * t * t10 + 0.2e1 * t * t18 + 0.4e1 * t10 * u + 0.2e1 * t18 * u + 0.2e1 * t21 * u + t29 * u + 0.2e1 * t7 * u;
  t44 = polylog(3, -u / (q4 - u));
  return(-0.4e1 / 0.3e1 * CF * Nc * (-0.9e1 * t - 0.7e1 + 0.9e1 * q4 + 0.9e1 * q3 - 0.9e1 * u) * t35 * Nf / t / t18 * t44);
}
double  GstarGstarMeNNLOSoft::TT14 (
  double u,
  double t,
  double q3,
  double q4,
  double z,
  double zp,
  double Nc,
  double Nf,
  double CF)
{
  double t10;
  double t18;
  double t21;
  double t29;
  double t35;
  double t44;
  double t7;
  t7 = q3 * q3;
  t10 = q3 * q4;
  t18 = u * u;
  t21 = q4 * q4;
  t29 = t * t;
  t35 = -0.2e1 * q3 * t * u - 0.2e1 * q4 * t * u - 0.4e1 * q3 * t18 - 0.4e1 * q4 * t18 - 0.2e1 * t * t10 + 0.2e1 * t * t18 + 0.4e1 * t10 * u + 0.2e1 * t18 * u + 0.2e1 * t21 * u + t29 * u + 0.2e1 * t7 * u;
  t44 = polylog(3, -u / (q3 - u));
  return(-0.4e1 / 0.3e1 * CF * Nc * (-0.9e1 * t - 0.7e1 + 0.9e1 * q4 + 0.9e1 * q3 - 0.9e1 * u) * t35 * Nf / t / t18 * t44);
}
double  GstarGstarMeNNLOSoft::TT13 (
  double u,
  double t,
  double q3,
  double q4,
  double z,
  double zp,
  double Nc,
  double Nf,
  double CF)
{
  double t11;
  double t12;
  double t18;
  double t27;
  double t45;
  double t51;
  double t7;
  t7 = q3 * q3;
  t11 = q3 * q4;
  t12 = t * t;
  t18 = u * u;
  t27 = q4 * q4;
  t45 = -0.6e1 * q3 * t * t18 - 0.6e1 * q3 * t12 * u - 0.6e1 * q4 * t * t18 - 0.6e1 * q4 * t12 * u + 0.8e1 * t * t11 * u + 0.3e1 * t * t12 * u + 0.3e1 * t * t18 * u + 0.4e1 * t * t27 * u + 0.4e1 * t * t7 * u - 0.2e1 * t11 * t12 - 0.2e1 * t11 * t18 + 0.4e1 * t12 * t18;
  t51 = polylog(3, zp);
  return(0.8e1 / 0.3e1 * Nf * (-0.3e1 * u - 0.2e1 - 0.3e1 * t + 0.3e1 * q4 + 0.3e1 * q3) * t45 * Nc * CF / t12 / t18 * t51);
}
double  GstarGstarMeNNLOSoft::TT12 (
  double u,
  double t,
  double q3,
  double q4,
  double z,
  double zp,
  double Nc,
  double Nf,
  double CF)
{
  double t11;
  double t12;
  double t18;
  double t27;
  double t45;
  double t51;
  double t7;
  t7 = q3 * q3;
  t11 = q3 * q4;
  t12 = t * t;
  t18 = u * u;
  t27 = q4 * q4;
  t45 = -0.6e1 * q3 * t * t18 - 0.6e1 * q3 * t12 * u - 0.6e1 * q4 * t * t18 - 0.6e1 * q4 * t12 * u + 0.8e1 * t * t11 * u + 0.3e1 * t * t12 * u + 0.3e1 * t * t18 * u + 0.4e1 * t * t27 * u + 0.4e1 * t * t7 * u - 0.2e1 * t11 * t12 - 0.2e1 * t11 * t18 + 0.4e1 * t12 * t18;
  t51 = polylog(3, z);
  return(0.8e1 / 0.3e1 * Nf * (-0.3e1 * u - 0.2e1 - 0.3e1 * t + 0.3e1 * q4 + 0.3e1 * q3) * t45 * Nc * CF / t12 / t18 * t51);
}
