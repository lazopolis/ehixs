switch(i+j*3+k*9)
{
case 0:
  a *= (0.5*C1*D1*D2*D3*Power(L,2) + D1*D2*(C3*D3 - 1.*C2*D3*L))*N0[r] + C2*D1*D2*D3*N1[r] + C1*D1*D2*D3*(-1.*L*N1[r] + N2[r]) + C0*(-0.16666666666666666*D1*D2*D3*Power(L,3)*N0[r] + D1*D2*D3*(L*(0.5*L*N1[r] - 1.*N2[r]) + N3[r])) + A2*(1.2020569031595942*A2*A2b*C0*D1*D2*D3*N0[r] - 1.*C1*D1*D3*N1[r] + N0[r]*(-1.*C2*D1*D3 + C1*D1*(-1.6449340668482262*A2b*D2*D3 + D3*(1.*L - 1.*R2))) + C0*(D1*(-1.6449340668482262*A2b*D2*D3*N1[r] + D3*(1.*L*N1[r] - 1.*N2[r] - 1.*N1[r]*R2)) + D1*N0[r]*(A2b*D2*(1.2020569031595942*A2b*D3 + 1.6449340668482262*D3*L) + D3*(-0.5*Power(R2,2) + L*(-0.5*L + 1.*R2)))) + A3*(C1*D1*N0[r] + C0*(D1*N1[r] + D1*N0[r]*(1.6449340668482262*A2b*D2 + 1.6449340668482262*A3b*D3 - 1.*L + R2 + R3)))) + A3*(1.2020569031595942*A3*A3b*C0*D1*D2*D3*N0[r] - 1.*C1*D1*D2*N1[r] + N0[r]*(-1.*C2*D1*D2 + C1*D1*D2*(-1.6449340668482262*A3b*D3 + 1.*L - 1.*R3)) + C0*(D1*D2*(-1.6449340668482262*A3b*D3*N1[r] + 1.*L*N1[r] - 1.*N2[r] - 1.*N1[r]*R3) + D1*D2*N0[r]*(A3b*D3*(1.2020569031595942*A3b + 1.6449340668482262*L) - 0.5*Power(R3,2) + L*(-0.5*L + 1.*R3)))) + A1*(1.2020569031595942*A1*A1b*C0*D1*D2*D3*N0[r] - 1.*C1*D2*D3*N1[r] + N0[r]*(-1.*C2*D2*D3 + C1*(-1.6449340668482262*A1b*D1*D2*D3 + D2*D3*(1.*L - 1.*R1))) + C0*(-1.6449340668482262*A1b*D1*D2*D3*N1[r] + D2*D3*(1.*L*N1[r] - 1.*N2[r] - 1.*N1[r]*R1) + N0[r]*(A1b*D1*D2*(1.2020569031595942*A1b*D3 + 1.6449340668482262*D3*L) + D2*D3*(-0.5*Power(R1,2) + L*(-0.5*L + 1.*R1)))) + A2*(-1.*A3*C0*N0[r] + C1*D3*N0[r] + C0*(D3*N1[r] + N0[r]*(1.6449340668482262*A1b*D1*D3 + 1.6449340668482262*A2b*D2*D3 + D3*(-1.*L + R1 + R2)))) + A3*(C1*D2*N0[r] + C0*(D2*N1[r] + N0[r]*(1.6449340668482262*A1b*D1*D2 + D2*(1.6449340668482262*A3b*D3 - 1.*L + R1 + R3)))));
  b *= A1*A2*A3*D1*D2*D3;
  break;
case 1:
  a *= (0.5*C1*D1b*D2*D3*Power(L,2) + D1b*D2*(C3*D3 - 1.*C2*D3*L))*N0[r] + C2*D1b*D2*D3*N1[r] + C1*D1b*D2*D3*(-1.*L*N1[r] + N2[r]) + C0*(-0.16666666666666666*D1b*D2*D3*Power(L,3)*N0[r] + D1b*D2*D3*(L*(0.5*L*N1[r] - 1.*N2[r]) + N3[r])) + A2*(1.2020569031595942*A2*A2b*C0*D1b*D2*D3*N0[r] - 1.*C1*D1b*D3*N1[r] + N0[r]*(-1.*C2*D1b*D3 + C1*D1b*(-1.6449340668482262*A2b*D2*D3 + D3*(1.*L - 1.*R2))) + C0*(D1b*(-1.6449340668482262*A2b*D2*D3*N1[r] + D3*(1.*L*N1[r] - 1.*N2[r] - 1.*N1[r]*R2)) + D1b*N0[r]*(A2b*D2*(1.2020569031595942*A2b*D3 + 1.6449340668482262*D3*L) + D3*(-0.5*Power(R2,2) + L*(-0.5*L + 1.*R2)))) + A3*(C1*D1b*N0[r] + C0*(D1b*N1[r] + D1b*N0[r]*(1.6449340668482262*A2b*D2 + 1.6449340668482262*A3b*D3 - 1.*L + R2 + R3)))) + A3*(1.2020569031595942*A3*A3b*C0*D1b*D2*D3*N0[r] - 1.*C1*D1b*D2*N1[r] + N0[r]*(-1.*C2*D1b*D2 + C1*D1b*D2*(-1.6449340668482262*A3b*D3 + 1.*L - 1.*R3)) + C0*(D1b*D2*(-1.6449340668482262*A3b*D3*N1[r] + 1.*L*N1[r] - 1.*N2[r] - 1.*N1[r]*R3) + D1b*D2*N0[r]*(A3b*D3*(1.2020569031595942*A3b + 1.6449340668482262*L) - 0.5*Power(R3,2) + L*(-0.5*L + 1.*R3)))) + A1b*(1.2020569031595942*A1*A1b*C0*D1b*D2*D3*N0[r] - 1.*C1*D2*D3*N1[r] + N0[r]*(-1.*C2*D2*D3 + C1*(-1.6449340668482262*A1*D1b*D2*D3 + D2*D3*(1.*L - 1.*R1))) + C0*(-1.6449340668482262*A1*D1b*D2*D3*N1[r] + D2*D3*(1.*L*N1[r] - 1.*N2[r] - 1.*N1[r]*R1) + N0[r]*(A1*D1b*D2*(1.2020569031595942*A1*D3 + 1.6449340668482262*D3*L) + D2*D3*(-0.5*Power(R1,2) + L*(-0.5*L + 1.*R1)))) + A2*(-1.*A3*C0*N0[r] + C1*D3*N0[r] + C0*(D3*N1[r] + N0[r]*(1.6449340668482262*A1*D1b*D3 + 1.6449340668482262*A2b*D2*D3 + D3*(-1.*L + R1 + R2)))) + A3*(C1*D2*N0[r] + C0*(D2*N1[r] + N0[r]*(1.6449340668482262*A1*D1b*D2 + D2*(1.6449340668482262*A3b*D3 - 1.*L + R1 + R3)))));
  b *= A1b*A2*A3*D1b*D2*D3;
  break;
case 2:
  a *= C1*D2*D3*N1[r] + N0[r]*(C2*D2*D3 + C1*D2*D3*(-1.*L + R1)) + C0*(D2*D3*(-1.*L*N1[r] + N2[r] + N1[r]*R1) + D2*D3*N0[r]*(L*(0.5*L - 1.*R1) + 0.5*Power(R1,2))) + A2*(A3*C0*N0[r] - 1.*C1*D3*N0[r] + C0*(-1.*D3*N1[r] + N0[r]*(-1.6449340668482262*A2b*D2*D3 + D3*(1.*L - 1.*R1 - 1.*R2)))) + A3*(-1.*C1*D2*N0[r] + C0*(-1.*D2*N1[r] + D2*N0[r]*(-1.6449340668482262*A3b*D3 + 1.*L - 1.*R1 - 1.*R3)));
  b *= A2*A3*D1*D1b*D2*D3;
  break;
case 3:
  a *= (0.5*C1*D1*D2b*D3*Power(L,2) + D1*D2b*(C3*D3 - 1.*C2*D3*L))*N0[r] + C2*D1*D2b*D3*N1[r] + C1*D1*D2b*D3*(-1.*L*N1[r] + N2[r]) + C0*(-0.16666666666666666*D1*D2b*D3*Power(L,3)*N0[r] + D1*D2b*D3*(L*(0.5*L*N1[r] - 1.*N2[r]) + N3[r])) + A2b*(1.2020569031595942*A2*A2b*C0*D1*D2b*D3*N0[r] - 1.*C1*D1*D3*N1[r] + N0[r]*(-1.*C2*D1*D3 + C1*D1*(-1.6449340668482262*A2*D2b*D3 + D3*(1.*L - 1.*R2))) + C0*(D1*(-1.6449340668482262*A2*D2b*D3*N1[r] + D3*(1.*L*N1[r] - 1.*N2[r] - 1.*N1[r]*R2)) + D1*N0[r]*(A2*D2b*(1.2020569031595942*A2*D3 + 1.6449340668482262*D3*L) + D3*(-0.5*Power(R2,2) + L*(-0.5*L + 1.*R2)))) + A3*(C1*D1*N0[r] + C0*(D1*N1[r] + D1*N0[r]*(1.6449340668482262*A2*D2b + 1.6449340668482262*A3b*D3 - 1.*L + R2 + R3)))) + A3*(1.2020569031595942*A3*A3b*C0*D1*D2b*D3*N0[r] - 1.*C1*D1*D2b*N1[r] + N0[r]*(-1.*C2*D1*D2b + C1*D1*D2b*(-1.6449340668482262*A3b*D3 + 1.*L - 1.*R3)) + C0*(D1*D2b*(-1.6449340668482262*A3b*D3*N1[r] + 1.*L*N1[r] - 1.*N2[r] - 1.*N1[r]*R3) + D1*D2b*N0[r]*(A3b*D3*(1.2020569031595942*A3b + 1.6449340668482262*L) - 0.5*Power(R3,2) + L*(-0.5*L + 1.*R3)))) + A1*(1.2020569031595942*A1*A1b*C0*D1*D2b*D3*N0[r] - 1.*C1*D2b*D3*N1[r] + N0[r]*(-1.*C2*D2b*D3 + C1*(-1.6449340668482262*A1b*D1*D2b*D3 + D2b*D3*(1.*L - 1.*R1))) + C0*(-1.6449340668482262*A1b*D1*D2b*D3*N1[r] + D2b*D3*(1.*L*N1[r] - 1.*N2[r] - 1.*N1[r]*R1) + N0[r]*(A1b*D1*D2b*(1.2020569031595942*A1b*D3 + 1.6449340668482262*D3*L) + D2b*D3*(-0.5*Power(R1,2) + L*(-0.5*L + 1.*R1)))) + A2b*(-1.*A3*C0*N0[r] + C1*D3*N0[r] + C0*(D3*N1[r] + N0[r]*(1.6449340668482262*A1b*D1*D3 + 1.6449340668482262*A2*D2b*D3 + D3*(-1.*L + R1 + R2)))) + A3*(C1*D2b*N0[r] + C0*(D2b*N1[r] + N0[r]*(1.6449340668482262*A1b*D1*D2b + D2b*(1.6449340668482262*A3b*D3 - 1.*L + R1 + R3)))));
  b *= A1*A2b*A3*D1*D2b*D3;
  break;
case 4:
  a *= (0.5*C1*D1b*D2b*D3*Power(L,2) + D1b*D2b*(C3*D3 - 1.*C2*D3*L))*N0[r] + C2*D1b*D2b*D3*N1[r] + C1*D1b*D2b*D3*(-1.*L*N1[r] + N2[r]) + C0*(-0.16666666666666666*D1b*D2b*D3*Power(L,3)*N0[r] + D1b*D2b*D3*(L*(0.5*L*N1[r] - 1.*N2[r]) + N3[r])) + A2b*(1.2020569031595942*A2*A2b*C0*D1b*D2b*D3*N0[r] - 1.*C1*D1b*D3*N1[r] + N0[r]*(-1.*C2*D1b*D3 + C1*D1b*(-1.6449340668482262*A2*D2b*D3 + D3*(1.*L - 1.*R2))) + C0*(D1b*(-1.6449340668482262*A2*D2b*D3*N1[r] + D3*(1.*L*N1[r] - 1.*N2[r] - 1.*N1[r]*R2)) + D1b*N0[r]*(A2*D2b*(1.2020569031595942*A2*D3 + 1.6449340668482262*D3*L) + D3*(-0.5*Power(R2,2) + L*(-0.5*L + 1.*R2)))) + A3*(C1*D1b*N0[r] + C0*(D1b*N1[r] + D1b*N0[r]*(1.6449340668482262*A2*D2b + 1.6449340668482262*A3b*D3 - 1.*L + R2 + R3)))) + A3*(1.2020569031595942*A3*A3b*C0*D1b*D2b*D3*N0[r] - 1.*C1*D1b*D2b*N1[r] + N0[r]*(-1.*C2*D1b*D2b + C1*D1b*D2b*(-1.6449340668482262*A3b*D3 + 1.*L - 1.*R3)) + C0*(D1b*D2b*(-1.6449340668482262*A3b*D3*N1[r] + 1.*L*N1[r] - 1.*N2[r] - 1.*N1[r]*R3) + D1b*D2b*N0[r]*(A3b*D3*(1.2020569031595942*A3b + 1.6449340668482262*L) - 0.5*Power(R3,2) + L*(-0.5*L + 1.*R3)))) + A1b*(1.2020569031595942*A1*A1b*C0*D1b*D2b*D3*N0[r] - 1.*C1*D2b*D3*N1[r] + N0[r]*(-1.*C2*D2b*D3 + C1*(-1.6449340668482262*A1*D1b*D2b*D3 + D2b*D3*(1.*L - 1.*R1))) + C0*(-1.6449340668482262*A1*D1b*D2b*D3*N1[r] + D2b*D3*(1.*L*N1[r] - 1.*N2[r] - 1.*N1[r]*R1) + N0[r]*(A1*D1b*D2b*(1.2020569031595942*A1*D3 + 1.6449340668482262*D3*L) + D2b*D3*(-0.5*Power(R1,2) + L*(-0.5*L + 1.*R1)))) + A2b*(-1.*A3*C0*N0[r] + C1*D3*N0[r] + C0*(D3*N1[r] + N0[r]*(1.6449340668482262*A1*D1b*D3 + 1.6449340668482262*A2*D2b*D3 + D3*(-1.*L + R1 + R2)))) + A3*(C1*D2b*N0[r] + C0*(D2b*N1[r] + N0[r]*(1.6449340668482262*A1*D1b*D2b + D2b*(1.6449340668482262*A3b*D3 - 1.*L + R1 + R3)))));
  b *= A1b*A2b*A3*D1b*D2b*D3;
  break;
case 5:
  a *= C1*D2b*D3*N1[r] + N0[r]*(C2*D2b*D3 + C1*D2b*D3*(-1.*L + R1)) + C0*(D2b*D3*(-1.*L*N1[r] + N2[r] + N1[r]*R1) + D2b*D3*N0[r]*(L*(0.5*L - 1.*R1) + 0.5*Power(R1,2))) + A2b*(A3*C0*N0[r] - 1.*C1*D3*N0[r] + C0*(-1.*D3*N1[r] + N0[r]*(-1.6449340668482262*A2*D2b*D3 + D3*(1.*L - 1.*R1 - 1.*R2)))) + A3*(-1.*C1*D2b*N0[r] + C0*(-1.*D2b*N1[r] + D2b*N0[r]*(-1.6449340668482262*A3b*D3 + 1.*L - 1.*R1 - 1.*R3)));
  b *= A2b*A3*D1*D1b*D2b*D3;
  break;
case 6:
  a *= C1*D1*D3*N1[r] + A1*(A3*C0*N0[r] - 1.*C1*D3*N0[r] + C0*(-1.*D3*N1[r] + N0[r]*(-1.6449340668482262*A1b*D1*D3 + D3*(1.*L - 1.*R1 - 1.*R2)))) + N0[r]*(C2*D1*D3 + C1*D1*D3*(-1.*L + R2)) + C0*(D1*D3*(-1.*L*N1[r] + N2[r] + N1[r]*R2) + D1*D3*N0[r]*(L*(0.5*L - 1.*R2) + 0.5*Power(R2,2))) + A3*(-1.*C1*D1*N0[r] + C0*(-1.*D1*N1[r] + D1*N0[r]*(-1.6449340668482262*A3b*D3 + 1.*L - 1.*R2 - 1.*R3)));
  b *= A1*A3*D1*D2*D2b*D3;
  break;
case 7:
  a *= C1*D1b*D3*N1[r] + A1b*(A3*C0*N0[r] - 1.*C1*D3*N0[r] + C0*(-1.*D3*N1[r] + N0[r]*(-1.6449340668482262*A1*D1b*D3 + D3*(1.*L - 1.*R1 - 1.*R2)))) + N0[r]*(C2*D1b*D3 + C1*D1b*D3*(-1.*L + R2)) + C0*(D1b*D3*(-1.*L*N1[r] + N2[r] + N1[r]*R2) + D1b*D3*N0[r]*(L*(0.5*L - 1.*R2) + 0.5*Power(R2,2))) + A3*(-1.*C1*D1b*N0[r] + C0*(-1.*D1b*N1[r] + D1b*N0[r]*(-1.6449340668482262*A3b*D3 + 1.*L - 1.*R2 - 1.*R3)));
  b *= A1b*A3*D1b*D2*D2b*D3;
  break;
case 8:
  a *= -1.*A3*C0*N0[r] + C1*D3*N0[r] + C0*(D3*N1[r] + D3*N0[r]*(-1.*L + R1 + R2));
  b *= A3*D1*D1b*D2*D2b*D3;
  break;
case 9:
  a *= (0.5*C1*D1*D2*D3b*Power(L,2) + D1*D2*(C3*D3b - 1.*C2*D3b*L))*N0[r] + C2*D1*D2*D3b*N1[r] + C1*D1*D2*D3b*(-1.*L*N1[r] + N2[r]) + C0*(-0.16666666666666666*D1*D2*D3b*Power(L,3)*N0[r] + D1*D2*D3b*(L*(0.5*L*N1[r] - 1.*N2[r]) + N3[r])) + A2*(1.2020569031595942*A2*A2b*C0*D1*D2*D3b*N0[r] - 1.*C1*D1*D3b*N1[r] + N0[r]*(-1.*C2*D1*D3b + C1*D1*(-1.6449340668482262*A2b*D2*D3b + D3b*(1.*L - 1.*R2))) + C0*(D1*(-1.6449340668482262*A2b*D2*D3b*N1[r] + D3b*(1.*L*N1[r] - 1.*N2[r] - 1.*N1[r]*R2)) + D1*N0[r]*(A2b*D2*(1.2020569031595942*A2b*D3b + 1.6449340668482262*D3b*L) + D3b*(-0.5*Power(R2,2) + L*(-0.5*L + 1.*R2)))) + A3b*(C1*D1*N0[r] + C0*(D1*N1[r] + D1*N0[r]*(1.6449340668482262*A2b*D2 + 1.6449340668482262*A3*D3b - 1.*L + R2 + R3)))) + A3b*(1.2020569031595942*A3*A3b*C0*D1*D2*D3b*N0[r] - 1.*C1*D1*D2*N1[r] + N0[r]*(-1.*C2*D1*D2 + C1*D1*D2*(-1.6449340668482262*A3*D3b + 1.*L - 1.*R3)) + C0*(D1*D2*(-1.6449340668482262*A3*D3b*N1[r] + 1.*L*N1[r] - 1.*N2[r] - 1.*N1[r]*R3) + D1*D2*N0[r]*(A3*D3b*(1.2020569031595942*A3 + 1.6449340668482262*L) - 0.5*Power(R3,2) + L*(-0.5*L + 1.*R3)))) + A1*(1.2020569031595942*A1*A1b*C0*D1*D2*D3b*N0[r] - 1.*C1*D2*D3b*N1[r] + N0[r]*(-1.*C2*D2*D3b + C1*(-1.6449340668482262*A1b*D1*D2*D3b + D2*D3b*(1.*L - 1.*R1))) + C0*(-1.6449340668482262*A1b*D1*D2*D3b*N1[r] + D2*D3b*(1.*L*N1[r] - 1.*N2[r] - 1.*N1[r]*R1) + N0[r]*(A1b*D1*D2*(1.2020569031595942*A1b*D3b + 1.6449340668482262*D3b*L) + D2*D3b*(-0.5*Power(R1,2) + L*(-0.5*L + 1.*R1)))) + A2*(-1.*A3b*C0*N0[r] + C1*D3b*N0[r] + C0*(D3b*N1[r] + N0[r]*(1.6449340668482262*A1b*D1*D3b + 1.6449340668482262*A2b*D2*D3b + D3b*(-1.*L + R1 + R2)))) + A3b*(C1*D2*N0[r] + C0*(D2*N1[r] + N0[r]*(1.6449340668482262*A1b*D1*D2 + D2*(1.6449340668482262*A3*D3b - 1.*L + R1 + R3)))));
  b *= A1*A2*A3b*D1*D2*D3b;
  break;
case 10:
  a *= (0.5*C1*D1b*D2*D3b*Power(L,2) + D1b*D2*(C3*D3b - 1.*C2*D3b*L))*N0[r] + C2*D1b*D2*D3b*N1[r] + C1*D1b*D2*D3b*(-1.*L*N1[r] + N2[r]) + C0*(-0.16666666666666666*D1b*D2*D3b*Power(L,3)*N0[r] + D1b*D2*D3b*(L*(0.5*L*N1[r] - 1.*N2[r]) + N3[r])) + A2*(1.2020569031595942*A2*A2b*C0*D1b*D2*D3b*N0[r] - 1.*C1*D1b*D3b*N1[r] + N0[r]*(-1.*C2*D1b*D3b + C1*D1b*(-1.6449340668482262*A2b*D2*D3b + D3b*(1.*L - 1.*R2))) + C0*(D1b*(-1.6449340668482262*A2b*D2*D3b*N1[r] + D3b*(1.*L*N1[r] - 1.*N2[r] - 1.*N1[r]*R2)) + D1b*N0[r]*(A2b*D2*(1.2020569031595942*A2b*D3b + 1.6449340668482262*D3b*L) + D3b*(-0.5*Power(R2,2) + L*(-0.5*L + 1.*R2)))) + A3b*(C1*D1b*N0[r] + C0*(D1b*N1[r] + D1b*N0[r]*(1.6449340668482262*A2b*D2 + 1.6449340668482262*A3*D3b - 1.*L + R2 + R3)))) + A3b*(1.2020569031595942*A3*A3b*C0*D1b*D2*D3b*N0[r] - 1.*C1*D1b*D2*N1[r] + N0[r]*(-1.*C2*D1b*D2 + C1*D1b*D2*(-1.6449340668482262*A3*D3b + 1.*L - 1.*R3)) + C0*(D1b*D2*(-1.6449340668482262*A3*D3b*N1[r] + 1.*L*N1[r] - 1.*N2[r] - 1.*N1[r]*R3) + D1b*D2*N0[r]*(A3*D3b*(1.2020569031595942*A3 + 1.6449340668482262*L) - 0.5*Power(R3,2) + L*(-0.5*L + 1.*R3)))) + A1b*(1.2020569031595942*A1*A1b*C0*D1b*D2*D3b*N0[r] - 1.*C1*D2*D3b*N1[r] + N0[r]*(-1.*C2*D2*D3b + C1*(-1.6449340668482262*A1*D1b*D2*D3b + D2*D3b*(1.*L - 1.*R1))) + C0*(-1.6449340668482262*A1*D1b*D2*D3b*N1[r] + D2*D3b*(1.*L*N1[r] - 1.*N2[r] - 1.*N1[r]*R1) + N0[r]*(A1*D1b*D2*(1.2020569031595942*A1*D3b + 1.6449340668482262*D3b*L) + D2*D3b*(-0.5*Power(R1,2) + L*(-0.5*L + 1.*R1)))) + A2*(-1.*A3b*C0*N0[r] + C1*D3b*N0[r] + C0*(D3b*N1[r] + N0[r]*(1.6449340668482262*A1*D1b*D3b + 1.6449340668482262*A2b*D2*D3b + D3b*(-1.*L + R1 + R2)))) + A3b*(C1*D2*N0[r] + C0*(D2*N1[r] + N0[r]*(1.6449340668482262*A1*D1b*D2 + D2*(1.6449340668482262*A3*D3b - 1.*L + R1 + R3)))));
  b *= A1b*A2*A3b*D1b*D2*D3b;
  break;
case 11:
  a *= C1*D2*D3b*N1[r] + N0[r]*(C2*D2*D3b + C1*D2*D3b*(-1.*L + R1)) + C0*(D2*D3b*(-1.*L*N1[r] + N2[r] + N1[r]*R1) + D2*D3b*N0[r]*(L*(0.5*L - 1.*R1) + 0.5*Power(R1,2))) + A2*(A3b*C0*N0[r] - 1.*C1*D3b*N0[r] + C0*(-1.*D3b*N1[r] + N0[r]*(-1.6449340668482262*A2b*D2*D3b + D3b*(1.*L - 1.*R1 - 1.*R2)))) + A3b*(-1.*C1*D2*N0[r] + C0*(-1.*D2*N1[r] + D2*N0[r]*(-1.6449340668482262*A3*D3b + 1.*L - 1.*R1 - 1.*R3)));
  b *= A2*A3b*D1*D1b*D2*D3b;
  break;
case 12:
  a *= (0.5*C1*D1*D2b*D3b*Power(L,2) + D1*D2b*(C3*D3b - 1.*C2*D3b*L))*N0[r] + C2*D1*D2b*D3b*N1[r] + C1*D1*D2b*D3b*(-1.*L*N1[r] + N2[r]) + C0*(-0.16666666666666666*D1*D2b*D3b*Power(L,3)*N0[r] + D1*D2b*D3b*(L*(0.5*L*N1[r] - 1.*N2[r]) + N3[r])) + A2b*(1.2020569031595942*A2*A2b*C0*D1*D2b*D3b*N0[r] - 1.*C1*D1*D3b*N1[r] + N0[r]*(-1.*C2*D1*D3b + C1*D1*(-1.6449340668482262*A2*D2b*D3b + D3b*(1.*L - 1.*R2))) + C0*(D1*(-1.6449340668482262*A2*D2b*D3b*N1[r] + D3b*(1.*L*N1[r] - 1.*N2[r] - 1.*N1[r]*R2)) + D1*N0[r]*(A2*D2b*(1.2020569031595942*A2*D3b + 1.6449340668482262*D3b*L) + D3b*(-0.5*Power(R2,2) + L*(-0.5*L + 1.*R2)))) + A3b*(C1*D1*N0[r] + C0*(D1*N1[r] + D1*N0[r]*(1.6449340668482262*A2*D2b + 1.6449340668482262*A3*D3b - 1.*L + R2 + R3)))) + A3b*(1.2020569031595942*A3*A3b*C0*D1*D2b*D3b*N0[r] - 1.*C1*D1*D2b*N1[r] + N0[r]*(-1.*C2*D1*D2b + C1*D1*D2b*(-1.6449340668482262*A3*D3b + 1.*L - 1.*R3)) + C0*(D1*D2b*(-1.6449340668482262*A3*D3b*N1[r] + 1.*L*N1[r] - 1.*N2[r] - 1.*N1[r]*R3) + D1*D2b*N0[r]*(A3*D3b*(1.2020569031595942*A3 + 1.6449340668482262*L) - 0.5*Power(R3,2) + L*(-0.5*L + 1.*R3)))) + A1*(1.2020569031595942*A1*A1b*C0*D1*D2b*D3b*N0[r] - 1.*C1*D2b*D3b*N1[r] + N0[r]*(-1.*C2*D2b*D3b + C1*(-1.6449340668482262*A1b*D1*D2b*D3b + D2b*D3b*(1.*L - 1.*R1))) + C0*(-1.6449340668482262*A1b*D1*D2b*D3b*N1[r] + D2b*D3b*(1.*L*N1[r] - 1.*N2[r] - 1.*N1[r]*R1) + N0[r]*(A1b*D1*D2b*(1.2020569031595942*A1b*D3b + 1.6449340668482262*D3b*L) + D2b*D3b*(-0.5*Power(R1,2) + L*(-0.5*L + 1.*R1)))) + A2b*(-1.*A3b*C0*N0[r] + C1*D3b*N0[r] + C0*(D3b*N1[r] + N0[r]*(1.6449340668482262*A1b*D1*D3b + 1.6449340668482262*A2*D2b*D3b + D3b*(-1.*L + R1 + R2)))) + A3b*(C1*D2b*N0[r] + C0*(D2b*N1[r] + N0[r]*(1.6449340668482262*A1b*D1*D2b + D2b*(1.6449340668482262*A3*D3b - 1.*L + R1 + R3)))));
  b *= A1*A2b*A3b*D1*D2b*D3b;
  break;
case 13:
  a *= (0.5*C1*D1b*D2b*D3b*Power(L,2) + D1b*D2b*(C3*D3b - 1.*C2*D3b*L))*N0[r] + C2*D1b*D2b*D3b*N1[r] + C1*D1b*D2b*D3b*(-1.*L*N1[r] + N2[r]) + C0*(-0.16666666666666666*D1b*D2b*D3b*Power(L,3)*N0[r] + D1b*D2b*D3b*(L*(0.5*L*N1[r] - 1.*N2[r]) + N3[r])) + A2b*(1.2020569031595942*A2*A2b*C0*D1b*D2b*D3b*N0[r] - 1.*C1*D1b*D3b*N1[r] + N0[r]*(-1.*C2*D1b*D3b + C1*D1b*(-1.6449340668482262*A2*D2b*D3b + D3b*(1.*L - 1.*R2))) + C0*(D1b*(-1.6449340668482262*A2*D2b*D3b*N1[r] + D3b*(1.*L*N1[r] - 1.*N2[r] - 1.*N1[r]*R2)) + D1b*N0[r]*(A2*D2b*(1.2020569031595942*A2*D3b + 1.6449340668482262*D3b*L) + D3b*(-0.5*Power(R2,2) + L*(-0.5*L + 1.*R2)))) + A3b*(C1*D1b*N0[r] + C0*(D1b*N1[r] + D1b*N0[r]*(1.6449340668482262*A2*D2b + 1.6449340668482262*A3*D3b - 1.*L + R2 + R3)))) + A3b*(1.2020569031595942*A3*A3b*C0*D1b*D2b*D3b*N0[r] - 1.*C1*D1b*D2b*N1[r] + N0[r]*(-1.*C2*D1b*D2b + C1*D1b*D2b*(-1.6449340668482262*A3*D3b + 1.*L - 1.*R3)) + C0*(D1b*D2b*(-1.6449340668482262*A3*D3b*N1[r] + 1.*L*N1[r] - 1.*N2[r] - 1.*N1[r]*R3) + D1b*D2b*N0[r]*(A3*D3b*(1.2020569031595942*A3 + 1.6449340668482262*L) - 0.5*Power(R3,2) + L*(-0.5*L + 1.*R3)))) + A1b*(1.2020569031595942*A1*A1b*C0*D1b*D2b*D3b*N0[r] - 1.*C1*D2b*D3b*N1[r] + N0[r]*(-1.*C2*D2b*D3b + C1*(-1.6449340668482262*A1*D1b*D2b*D3b + D2b*D3b*(1.*L - 1.*R1))) + C0*(-1.6449340668482262*A1*D1b*D2b*D3b*N1[r] + D2b*D3b*(1.*L*N1[r] - 1.*N2[r] - 1.*N1[r]*R1) + N0[r]*(A1*D1b*D2b*(1.2020569031595942*A1*D3b + 1.6449340668482262*D3b*L) + D2b*D3b*(-0.5*Power(R1,2) + L*(-0.5*L + 1.*R1)))) + A2b*(-1.*A3b*C0*N0[r] + C1*D3b*N0[r] + C0*(D3b*N1[r] + N0[r]*(1.6449340668482262*A1*D1b*D3b + 1.6449340668482262*A2*D2b*D3b + D3b*(-1.*L + R1 + R2)))) + A3b*(C1*D2b*N0[r] + C0*(D2b*N1[r] + N0[r]*(1.6449340668482262*A1*D1b*D2b + D2b*(1.6449340668482262*A3*D3b - 1.*L + R1 + R3)))));
  b *= A1b*A2b*A3b*D1b*D2b*D3b;
  break;
case 14:
  a *= C1*D2b*D3b*N1[r] + N0[r]*(C2*D2b*D3b + C1*D2b*D3b*(-1.*L + R1)) + C0*(D2b*D3b*(-1.*L*N1[r] + N2[r] + N1[r]*R1) + D2b*D3b*N0[r]*(L*(0.5*L - 1.*R1) + 0.5*Power(R1,2))) + A2b*(A3b*C0*N0[r] - 1.*C1*D3b*N0[r] + C0*(-1.*D3b*N1[r] + N0[r]*(-1.6449340668482262*A2*D2b*D3b + D3b*(1.*L - 1.*R1 - 1.*R2)))) + A3b*(-1.*C1*D2b*N0[r] + C0*(-1.*D2b*N1[r] + D2b*N0[r]*(-1.6449340668482262*A3*D3b + 1.*L - 1.*R1 - 1.*R3)));
  b *= A2b*A3b*D1*D1b*D2b*D3b;
  break;
case 15:
  a *= C1*D1*D3b*N1[r] + A1*(A3b*C0*N0[r] - 1.*C1*D3b*N0[r] + C0*(-1.*D3b*N1[r] + N0[r]*(-1.6449340668482262*A1b*D1*D3b + D3b*(1.*L - 1.*R1 - 1.*R2)))) + N0[r]*(C2*D1*D3b + C1*D1*D3b*(-1.*L + R2)) + C0*(D1*D3b*(-1.*L*N1[r] + N2[r] + N1[r]*R2) + D1*D3b*N0[r]*(L*(0.5*L - 1.*R2) + 0.5*Power(R2,2))) + A3b*(-1.*C1*D1*N0[r] + C0*(-1.*D1*N1[r] + D1*N0[r]*(-1.6449340668482262*A3*D3b + 1.*L - 1.*R2 - 1.*R3)));
  b *= A1*A3b*D1*D2*D2b*D3b;
  break;
case 16:
  a *= C1*D1b*D3b*N1[r] + A1b*(A3b*C0*N0[r] - 1.*C1*D3b*N0[r] + C0*(-1.*D3b*N1[r] + N0[r]*(-1.6449340668482262*A1*D1b*D3b + D3b*(1.*L - 1.*R1 - 1.*R2)))) + N0[r]*(C2*D1b*D3b + C1*D1b*D3b*(-1.*L + R2)) + C0*(D1b*D3b*(-1.*L*N1[r] + N2[r] + N1[r]*R2) + D1b*D3b*N0[r]*(L*(0.5*L - 1.*R2) + 0.5*Power(R2,2))) + A3b*(-1.*C1*D1b*N0[r] + C0*(-1.*D1b*N1[r] + D1b*N0[r]*(-1.6449340668482262*A3*D3b + 1.*L - 1.*R2 - 1.*R3)));
  b *= A1b*A3b*D1b*D2*D2b*D3b;
  break;
case 17:
  a *= -1.*A3b*C0*N0[r] + C1*D3b*N0[r] + C0*(D3b*N1[r] + D3b*N0[r]*(-1.*L + R1 + R2));
  b *= A3b*D1*D1b*D2*D2b*D3b;
  break;
case 18:
  a *= C1*D1*D2*N1[r] + A1*(A2*C0*N0[r] - 1.*C1*D2*N0[r] + C0*(-1.*D2*N1[r] + N0[r]*(-1.6449340668482262*A1b*D1*D2 + D2*(1.*L - 1.*R1 - 1.*R3)))) + A2*(-1.*C1*D1*N0[r] + C0*(-1.*D1*N1[r] + D1*N0[r]*(-1.6449340668482262*A2b*D2 + 1.*L - 1.*R2 - 1.*R3))) + N0[r]*(C2*D1*D2 + C1*D1*D2*(-1.*L + R3)) + C0*(D1*D2*(-1.*L*N1[r] + N2[r] + N1[r]*R3) + D1*D2*N0[r]*(L*(0.5*L - 1.*R3) + 0.5*Power(R3,2)));
  b *= A1*A2*D1*D2*D3*D3b;
  break;
case 19:
  a *= C1*D1b*D2*N1[r] + A1b*(A2*C0*N0[r] - 1.*C1*D2*N0[r] + C0*(-1.*D2*N1[r] + N0[r]*(-1.6449340668482262*A1*D1b*D2 + D2*(1.*L - 1.*R1 - 1.*R3)))) + A2*(-1.*C1*D1b*N0[r] + C0*(-1.*D1b*N1[r] + D1b*N0[r]*(-1.6449340668482262*A2b*D2 + 1.*L - 1.*R2 - 1.*R3))) + N0[r]*(C2*D1b*D2 + C1*D1b*D2*(-1.*L + R3)) + C0*(D1b*D2*(-1.*L*N1[r] + N2[r] + N1[r]*R3) + D1b*D2*N0[r]*(L*(0.5*L - 1.*R3) + 0.5*Power(R3,2)));
  b *= A1b*A2*D1b*D2*D3*D3b;
  break;
case 20:
  a *= -1.*A2*C0*N0[r] + C1*D2*N0[r] + C0*(D2*N1[r] + D2*N0[r]*(-1.*L + R1 + R3));
  b *= A2*D1*D1b*D2*D3*D3b;
  break;
case 21:
  a *= C1*D1*D2b*N1[r] + A1*(A2b*C0*N0[r] - 1.*C1*D2b*N0[r] + C0*(-1.*D2b*N1[r] + N0[r]*(-1.6449340668482262*A1b*D1*D2b + D2b*(1.*L - 1.*R1 - 1.*R3)))) + A2b*(-1.*C1*D1*N0[r] + C0*(-1.*D1*N1[r] + D1*N0[r]*(-1.6449340668482262*A2*D2b + 1.*L - 1.*R2 - 1.*R3))) + N0[r]*(C2*D1*D2b + C1*D1*D2b*(-1.*L + R3)) + C0*(D1*D2b*(-1.*L*N1[r] + N2[r] + N1[r]*R3) + D1*D2b*N0[r]*(L*(0.5*L - 1.*R3) + 0.5*Power(R3,2)));
  b *= A1*A2b*D1*D2b*D3*D3b;
  break;
case 22:
  a *= C1*D1b*D2b*N1[r] + A1b*(A2b*C0*N0[r] - 1.*C1*D2b*N0[r] + C0*(-1.*D2b*N1[r] + N0[r]*(-1.6449340668482262*A1*D1b*D2b + D2b*(1.*L - 1.*R1 - 1.*R3)))) + A2b*(-1.*C1*D1b*N0[r] + C0*(-1.*D1b*N1[r] + D1b*N0[r]*(-1.6449340668482262*A2*D2b + 1.*L - 1.*R2 - 1.*R3))) + N0[r]*(C2*D1b*D2b + C1*D1b*D2b*(-1.*L + R3)) + C0*(D1b*D2b*(-1.*L*N1[r] + N2[r] + N1[r]*R3) + D1b*D2b*N0[r]*(L*(0.5*L - 1.*R3) + 0.5*Power(R3,2)));
  b *= A1b*A2b*D1b*D2b*D3*D3b;
  break;
case 23:
  a *= -1.*A2b*C0*N0[r] + C1*D2b*N0[r] + C0*(D2b*N1[r] + D2b*N0[r]*(-1.*L + R1 + R3));
  b *= A2b*D1*D1b*D2b*D3*D3b;
  break;
case 24:
  a *= -1.*A1*C0*N0[r] + C1*D1*N0[r] + C0*(D1*N1[r] + D1*N0[r]*(-1.*L + R2 + R3));
  b *= A1*D1*D2*D2b*D3*D3b;
  break;
case 25:
  a *= -1.*A1b*C0*N0[r] + C1*D1b*N0[r] + C0*(D1b*N1[r] + D1b*N0[r]*(-1.*L + R2 + R3));
  b *= A1b*D1b*D2*D2b*D3*D3b;
  break;
case 26:
  a *= C0*N0[r];
  b *= D1*D1b*D2*D2b*D3*D3b;
  break;
}
