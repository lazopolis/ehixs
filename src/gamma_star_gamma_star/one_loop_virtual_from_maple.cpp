#include <math.h>
//{q3,q4,u,t}
double GstarVCoeffsC1em1 (
  const vector<double>& kk)
{

  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return(-0.32e2 / u / t * q3 * q3 + (0.32e2 * pow(u, -0.2e1) - 0.64e2 / u / t) * q3 * q4 + (0.32e2 / u + 0.64e2 / t) * q3 - 0.32e2 / u / t * q4 * q4 + (0.32e2 / u + 0.64e2 / t) * q4 - 0.16e2 * t / u - 0.32e2 - 0.32e2 * u / t);
}
#include <math.h>

double GstarVCoeffsC1e0 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return(0.16e2 * ((-0.44e2 * t * u - 0.188e3 * u * u) * pow(q3, 0.5e1) * q4 * q4 + (-0.18e2 * t * t * u - 0.44e2 * t * u * u - 0.42e2 * pow(u, 0.3e1)) * pow(q3, 0.5e1) * q4 + (0.140e3 * t * u - 0.292e3 * u * u) * pow(q3, 0.4e1) * pow(q4, 0.3e1) + (0.40e2 * pow(t, 0.3e1) + 0.78e2 * t * t * u + 0.196e3 * t * u * u + 0.254e3 * pow(u, 0.3e1)) * pow(q3, 0.4e1) * q4 * q4 + (0.20e2 * pow(t, 0.3e1) * u + 0.112e3 * t * t * u * u + 0.216e3 * t * pow(u, 0.3e1) + 0.156e3 * pow(u, 0.4e1)) * pow(q3, 0.4e1) * q4 + (-0.24e2 * t * t * u + 0.40e2 * t * u * u + 0.336e3 * pow(u, 0.3e1)) * pow(q3, 0.3e1) * pow(q4, 0.3e1) + (-0.72e2 * pow(t, 0.3e1) * u - 0.124e3 * t * t * u * u - 0.48e2 * t * pow(u, 0.3e1) - 0.44e2 * pow(u, 0.4e1)) * pow(q3, 0.3e1) * q4 * q4 + (-0.5e1 * pow(t, 0.5e1) - 0.32e2 * pow(t, 0.4e1) * u - 0.142e3 * pow(t, 0.3e1) * u * u - 0.336e3 * t * t * pow(u, 0.3e1) - 0.429e3 * t * pow(u, 0.4e1) - 0.224e3 * pow(u, 0.5e1)) * pow(q3, 0.3e1) * q4 + (0.80e2 * t * t * u * u + 0.44e2 * t * pow(u, 0.3e1) - 0.52e2 * pow(u, 0.4e1)) * q3 * q3 * pow(q4, 0.3e1) + (0.3e1 * pow(t, 0.4e1) * u - 0.28e2 * pow(t, 0.3e1) * u * u - 0.160e3 * t * t * pow(u, 0.3e1) - 0.304e3 * t * pow(u, 0.4e1) - 0.143e3 * pow(u, 0.5e1)) * q3 * q3 * q4 * q4 + (0.10e2 * pow(t, 0.5e1) * u + 0.84e2 * pow(t, 0.4e1) * u * u + 0.224e3 * pow(t, 0.3e1) * pow(u, 0.3e1) + 0.336e3 * t * t * pow(u, 0.4e1) + 0.322e3 * t * pow(u, 0.5e1) + 0.136e3 * pow(u, 0.6e1)) * q3 * q3 * q4 + (-0.48e2 * t * t * pow(u, 0.3e1) - 0.96e2 * t * pow(u, 0.4e1) - 0.48e2 * pow(u, 0.5e1)) * q3 * pow(q4, 0.3e1) + (-0.10e2 * pow(t, 0.4e1) * u * u + 0.16e2 * pow(t, 0.3e1) * pow(u, 0.3e1) + 0.116e3 * t * t * pow(u, 0.4e1) + 0.156e3 * t * pow(u, 0.5e1) + 0.66e2 * pow(u, 0.6e1)) * q3 * q4 * q4 + (0.3e1 * pow(t, 0.5e1) * u * u - 0.2e1 * pow(t, 0.4e1) * pow(u, 0.3e1) + 0.2e1 * pow(t, 0.3e1) * pow(u, 0.4e1) + 0.6e1 * t * t * pow(u, 0.5e1) - 0.17e2 * t * pow(u, 0.6e1) - 0.16e2 * pow(u, 0.7e1)) * q3 * q4 + (-0.80e2 * t + 0.64e2 * u) * pow(q3, 0.5e1) * pow(q4, 0.3e1) + 0.48e2 * pow(q3, 0.4e1) * pow(q4, 0.4e1) * u - 0.160e3 * pow(q3, 0.3e1) * pow(q4, 0.4e1) * u * u + 0.96e2 * q3 * q3 * pow(q4, 0.4e1) * pow(u, 0.3e1) + 0.48e2 * pow(q3, 0.6e1) * q4 * q4 * u + 0.6e1 * pow(u, 0.9e1) + 0.64e2 * t * t * pow(u, 0.7e1) + 0.30e2 * t * pow(u, 0.8e1) + 0.4e1 * pow(t, 0.6e1) * pow(u, 0.3e1) + 0.22e2 * pow(t, 0.5e1) * pow(u, 0.4e1) + 0.54e2 * pow(t, 0.4e1) * pow(u, 0.5e1) + 0.76e2 * pow(t, 0.3e1) * pow(u, 0.6e1) + (pow(t, 0.6e1) * u + 0.12e2 * pow(t, 0.5e1) * u * u + 0.64e2 * pow(t, 0.4e1) * pow(u, 0.3e1) + 0.164e3 * pow(t, 0.3e1) * pow(u, 0.4e1) + 0.211e3 * t * t * pow(u, 0.5e1) + 0.132e3 * t * pow(u, 0.6e1) + 0.32e2 * pow(u, 0.7e1)) * q3 * q3 + (-0.6e1 * pow(t, 0.6e1) * u * u - 0.38e2 * pow(t, 0.5e1) * pow(u, 0.3e1) - 0.114e3 * pow(t, 0.4e1) * pow(u, 0.4e1) - 0.196e3 * pow(t, 0.3e1) * pow(u, 0.5e1) - 0.194e3 * t * t * pow(u, 0.6e1) - 0.102e3 * t * pow(u, 0.7e1) - 0.22e2 * pow(u, 0.8e1)) * q3 + (0.6e1 * pow(t, 0.4e1) * pow(u, 0.3e1) + 0.24e2 * pow(t, 0.3e1) * pow(u, 0.4e1) + 0.36e2 * t * t * pow(u, 0.5e1) + 0.24e2 * t * pow(u, 0.6e1) + 0.6e1 * pow(u, 0.7e1)) * q4 * q4 + (-0.6e1 * pow(t, 0.5e1) * pow(u, 0.3e1) - 0.34e2 * pow(t, 0.4e1) * pow(u, 0.4e1) - 0.76e2 * pow(t, 0.3e1) * pow(u, 0.5e1) - 0.84e2 * t * t * pow(u, 0.6e1) - 0.46e2 * t * pow(u, 0.7e1) - 0.10e2 * pow(u, 0.8e1)) * q4 + (0.8e1 * pow(t, 0.3e1) * u * u + 0.22e2 * t * t * pow(u, 0.3e1) + 0.20e2 * t * pow(u, 0.4e1) + 0.6e1 * pow(u, 0.5e1)) * pow(q3, 0.4e1) + (-0.10e2 * pow(t, 0.4e1) * u * u - 0.56e2 * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.104e3 * t * t * pow(u, 0.4e1) - 0.80e2 * t * pow(u, 0.5e1) - 0.22e2 * pow(u, 0.6e1)) * pow(q3, 0.3e1)) * pow(q3 - u, -0.2e1) * pow(u, -0.2e1) / t * pow(0.4e1 * q3 * q4 - t * t - 0.2e1 * t * u - u * u, -0.2e1));
}
#include <math.h>

double GstarVCoeffsC1e1 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return(-0.16e2 * (0.48e2 * pow(q3, 0.4e1) * pow(q4, 0.4e1) * u - 0.128e3 * pow(q3, 0.3e1) * pow(q4, 0.4e1) * u * u + 0.64e2 * q3 * q3 * pow(q4, 0.4e1) * pow(u, 0.3e1) + 0.48e2 * pow(q3, 0.6e1) * q4 * q4 * u + 0.7e1 * pow(u, 0.9e1) + (-0.96e2 * t + 0.48e2 * u) * pow(q3, 0.5e1) * pow(q4, 0.3e1) + (-0.32e2 * t * u - 0.144e3 * u * u) * pow(q3, 0.5e1) * q4 * q4 + (-0.24e2 * t * t * u - 0.40e2 * t * u * u - 0.32e2 * pow(u, 0.3e1)) * pow(q3, 0.5e1) * q4 + (0.200e3 * t * u - 0.136e3 * u * u) * pow(q3, 0.4e1) * pow(q4, 0.3e1) + (0.48e2 * pow(t, 0.3e1) + 0.84e2 * t * t * u + 0.176e3 * t * u * u + 0.220e3 * pow(u, 0.3e1)) * pow(q3, 0.4e1) * q4 * q4 + (0.22e2 * pow(t, 0.3e1) * u + 0.94e2 * t * t * u * u + 0.138e3 * t * pow(u, 0.3e1) + 0.98e2 * pow(u, 0.4e1)) * pow(q3, 0.4e1) * q4 + (-0.24e2 * t * t * u - 0.48e2 * t * u * u + 0.120e3 * pow(u, 0.3e1)) * pow(q3, 0.3e1) * pow(q4, 0.3e1) + (-0.100e3 * pow(t, 0.3e1) * u - 0.240e3 * t * t * u * u - 0.236e3 * t * pow(u, 0.3e1) - 0.176e3 * pow(u, 0.4e1)) * pow(q3, 0.3e1) * q4 * q4 + (-0.6e1 * pow(t, 0.5e1) - 0.31e2 * pow(t, 0.4e1) * u - 0.134e3 * pow(t, 0.3e1) * u * u - 0.280e3 * t * t * pow(u, 0.3e1) - 0.308e3 * t * pow(u, 0.4e1) - 0.153e3 * pow(u, 0.5e1)) * pow(q3, 0.3e1) * q4 + (0.64e2 * t * t * u * u + 0.56e2 * t * pow(u, 0.3e1) + 0.8e1 * pow(u, 0.4e1)) * q3 * q3 * pow(q4, 0.3e1) + (0.3e1 * pow(t, 0.4e1) * u + 0.12e2 * pow(t, 0.3e1) * u * u + 0.30e2 * t * t * pow(u, 0.3e1) - 0.4e1 * t * pow(u, 0.4e1) + 0.23e2 * pow(u, 0.5e1)) * q3 * q3 * q4 * q4 + (0.13e2 * pow(t, 0.5e1) * u + 0.103e3 * pow(t, 0.4e1) * u * u + 0.284e3 * pow(t, 0.3e1) * pow(u, 0.3e1) + 0.416e3 * t * t * pow(u, 0.4e1) + 0.359e3 * t * pow(u, 0.5e1) + 0.137e3 * pow(u, 0.6e1)) * q3 * q3 * q4 + (-0.32e2 * t * t * pow(u, 0.3e1) - 0.64e2 * t * pow(u, 0.4e1) - 0.32e2 * pow(u, 0.5e1)) * q3 * pow(q4, 0.3e1) + (-0.8e1 * pow(t, 0.4e1) * u * u + 0.4e1 * pow(t, 0.3e1) * pow(u, 0.3e1) + 0.48e2 * t * t * pow(u, 0.4e1) + 0.60e2 * t * pow(u, 0.5e1) + 0.24e2 * pow(u, 0.6e1)) * q3 * q4 * q4 + (-pow(t, 0.5e1) * u * u - 0.36e2 * pow(t, 0.4e1) * pow(u, 0.3e1) - 0.104e3 * pow(t, 0.3e1) * pow(u, 0.4e1) - 0.150e3 * t * t * pow(u, 0.5e1) - 0.127e3 * t * pow(u, 0.6e1) - 0.46e2 * pow(u, 0.7e1)) * q3 * q4 + 0.75e2 * t * t * pow(u, 0.7e1) + 0.35e2 * t * pow(u, 0.8e1) + 0.5e1 * pow(t, 0.6e1) * pow(u, 0.3e1) + 0.27e2 * pow(t, 0.5e1) * pow(u, 0.4e1) + 0.65e2 * pow(t, 0.4e1) * pow(u, 0.5e1) + 0.90e2 * pow(t, 0.3e1) * pow(u, 0.6e1) + (pow(t, 0.6e1) * u + 0.14e2 * pow(t, 0.5e1) * u * u + 0.59e2 * pow(t, 0.4e1) * pow(u, 0.3e1) + 0.122e3 * pow(t, 0.3e1) * pow(u, 0.4e1) + 0.137e3 * t * t * pow(u, 0.5e1) + 0.80e2 * t * pow(u, 0.6e1) + 0.19e2 * pow(u, 0.7e1)) * q3 * q3 + (-0.7e1 * pow(t, 0.6e1) * u * u - 0.43e2 * pow(t, 0.5e1) * pow(u, 0.3e1) - 0.118e3 * pow(t, 0.4e1) * pow(u, 0.4e1) - 0.184e3 * pow(t, 0.3e1) * pow(u, 0.5e1) - 0.169e3 * t * t * pow(u, 0.6e1) - 0.85e2 * t * pow(u, 0.7e1) - 0.18e2 * pow(u, 0.8e1)) * q3 + (0.4e1 * pow(t, 0.4e1) * pow(u, 0.3e1) + 0.16e2 * pow(t, 0.3e1) * pow(u, 0.4e1) + 0.24e2 * t * t * pow(u, 0.5e1) + 0.16e2 * t * pow(u, 0.6e1) + 0.4e1 * pow(u, 0.7e1)) * q4 * q4 + (-0.4e1 * pow(t, 0.5e1) * pow(u, 0.3e1) - 0.20e2 * pow(t, 0.4e1) * pow(u, 0.4e1) - 0.40e2 * pow(t, 0.3e1) * pow(u, 0.5e1) - 0.40e2 * t * t * pow(u, 0.6e1) - 0.20e2 * t * pow(u, 0.7e1) - 0.4e1 * pow(u, 0.8e1)) * q4 + (0.2e1 * pow(t, 0.4e1) * u + 0.10e2 * pow(t, 0.3e1) * u * u + 0.18e2 * t * t * pow(u, 0.3e1) + 0.14e2 * t * pow(u, 0.4e1) + 0.4e1 * pow(u, 0.5e1)) * pow(q3, 0.4e1) + (-0.2e1 * pow(t, 0.5e1) * u - 0.14e2 * pow(t, 0.4e1) * u * u - 0.42e2 * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.62e2 * t * t * pow(u, 0.4e1) - 0.44e2 * t * pow(u, 0.5e1) - 0.12e2 * pow(u, 0.6e1)) * pow(q3, 0.3e1)) * pow(q3 - u, -0.2e1) * pow(u, -0.2e1) / t * pow(0.4e1 * q3 * q4 - t * t - 0.2e1 * t * u - u * u, -0.2e1));
}
#include <math.h>

double GstarVCoeffsC1e2 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return(0.16e2 * (0.8e1 * u * pow(q3, 0.4e1) * q4 + (-0.20e2 * t + 0.4e1 * u) * pow(q3, 0.3e1) * q4 * q4 + (-0.12e2 * t * u - 0.12e2 * u * u) * pow(q3, 0.3e1) * q4 + (-0.4e1 * t * t * u - 0.4e1 * t * u * u) * pow(q3, 0.3e1) + 0.8e1 * u * q3 * q3 * pow(q4, 0.3e1) + (0.12e2 * t * u + 0.12e2 * u * u) * q3 * q3 * q4 * q4 + (0.5e1 * pow(t, 0.3e1) + 0.19e2 * t * t * u + 0.15e2 * t * u * u + 0.25e2 * pow(u, 0.3e1)) * q3 * q3 * q4 + (0.4e1 * pow(t, 0.3e1) * u + 0.12e2 * t * t * u * u + 0.8e1 * t * pow(u, 0.3e1)) * q3 * q3 + (-0.2e1 * t * t * u - 0.8e1 * t * u * u + 0.2e1 * pow(u, 0.3e1)) * q3 * q4 * q4 + (-0.2e1 * pow(t, 0.3e1) * u - 0.14e2 * t * t * u * u - 0.30e2 * t * pow(u, 0.3e1) - 0.26e2 * pow(u, 0.4e1)) * q3 * q4 + (-0.3e1 * pow(t, 0.4e1) * u - 0.10e2 * pow(t, 0.3e1) * u * u - 0.16e2 * t * t * pow(u, 0.3e1) - 0.14e2 * t * pow(u, 0.4e1) - 0.5e1 * pow(u, 0.5e1)) * q3 + 0.2e1 * pow(t, 0.4e1) * u * u + 0.9e1 * pow(t, 0.3e1) * pow(u, 0.3e1) + 0.17e2 * t * t * pow(u, 0.4e1) + 0.15e2 * t * pow(u, 0.5e1) + 0.5e1 * pow(u, 0.6e1)) / (0.4e1 * q3 * q4 - t * t - 0.2e1 * t * u - u * u) / t * pow(u, -0.2e1) / (q3 - u));
}
#include <math.h>

double GstarVCoeffsC2em1 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return(-0.32e2 / u / t * q3 * q3 + (-0.64e2 / u / t + 0.32e2 * pow(t, -0.2e1)) * q3 * q4 + (0.64e2 / u + 0.32e2 / t) * q3 - 0.32e2 / u / t * q4 * q4 + (0.64e2 / u + 0.32e2 / t) * q4 - 0.32e2 * t / u - 0.32e2 - 0.16e2 * u / t);
}
#include <math.h>

double GstarVCoeffsC2e0 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return(0.16e2 * (0.30e2 * pow(t, 0.8e1) * u + 0.64e2 * pow(t, 0.7e1) * u * u + (0.6e1 * pow(t, 0.7e1) + 0.24e2 * pow(t, 0.6e1) * u + 0.36e2 * pow(t, 0.5e1) * u * u + 0.24e2 * pow(t, 0.4e1) * pow(u, 0.3e1) + 0.6e1 * pow(t, 0.3e1) * pow(u, 0.4e1)) * q3 * q3 + (-0.10e2 * pow(t, 0.8e1) - 0.46e2 * pow(t, 0.7e1) * u - 0.84e2 * pow(t, 0.6e1) * u * u - 0.76e2 * pow(t, 0.5e1) * pow(u, 0.3e1) - 0.34e2 * pow(t, 0.4e1) * pow(u, 0.4e1) - 0.6e1 * pow(t, 0.3e1) * pow(u, 0.5e1)) * q3 + (0.6e1 * pow(t, 0.5e1) + 0.20e2 * pow(t, 0.4e1) * u + 0.22e2 * pow(t, 0.3e1) * u * u + 0.8e1 * t * t * pow(u, 0.3e1)) * pow(q4, 0.4e1) + (-0.22e2 * pow(t, 0.6e1) - 0.80e2 * pow(t, 0.5e1) * u - 0.104e3 * pow(t, 0.4e1) * u * u - 0.56e2 * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.10e2 * t * t * pow(u, 0.4e1)) * pow(q4, 0.3e1) + (0.32e2 * pow(t, 0.7e1) + 0.132e3 * pow(t, 0.6e1) * u + 0.211e3 * pow(t, 0.5e1) * u * u + 0.164e3 * pow(t, 0.4e1) * pow(u, 0.3e1) + 0.64e2 * pow(t, 0.3e1) * pow(u, 0.4e1) + 0.12e2 * t * t * pow(u, 0.5e1) + t * pow(u, 0.6e1)) * q4 * q4 + (-0.22e2 * pow(t, 0.8e1) - 0.102e3 * pow(t, 0.7e1) * u - 0.194e3 * pow(t, 0.6e1) * u * u - 0.196e3 * pow(t, 0.5e1) * pow(u, 0.3e1) - 0.114e3 * pow(t, 0.4e1) * pow(u, 0.4e1) - 0.38e2 * pow(t, 0.3e1) * pow(u, 0.5e1) - 0.6e1 * t * t * pow(u, 0.6e1)) * q4 + 0.6e1 * pow(t, 0.9e1) + 0.76e2 * pow(t, 0.6e1) * pow(u, 0.3e1) + 0.54e2 * pow(t, 0.5e1) * pow(u, 0.4e1) + 0.22e2 * pow(t, 0.4e1) * pow(u, 0.5e1) + 0.4e1 * pow(t, 0.3e1) * pow(u, 0.6e1) + (0.64e2 * t - 0.80e2 * u) * pow(q3, 0.3e1) * pow(q4, 0.5e1) + (-0.292e3 * t * t + 0.140e3 * t * u) * pow(q3, 0.3e1) * pow(q4, 0.4e1) + (0.336e3 * pow(t, 0.3e1) + 0.40e2 * t * t * u - 0.24e2 * t * u * u) * pow(q3, 0.3e1) * pow(q4, 0.3e1) + (-0.52e2 * pow(t, 0.4e1) + 0.44e2 * pow(t, 0.3e1) * u + 0.80e2 * t * t * u * u) * pow(q3, 0.3e1) * q4 * q4 + (-0.48e2 * pow(t, 0.5e1) - 0.96e2 * pow(t, 0.4e1) * u - 0.48e2 * pow(t, 0.3e1) * u * u) * pow(q3, 0.3e1) * q4 + (-0.188e3 * t * t - 0.44e2 * t * u) * q3 * q3 * pow(q4, 0.5e1) + (0.254e3 * pow(t, 0.3e1) + 0.196e3 * t * t * u + 0.78e2 * t * u * u + 0.40e2 * pow(u, 0.3e1)) * q3 * q3 * pow(q4, 0.4e1) + (-0.44e2 * pow(t, 0.4e1) - 0.48e2 * pow(t, 0.3e1) * u - 0.124e3 * t * t * u * u - 0.72e2 * t * pow(u, 0.3e1)) * q3 * q3 * pow(q4, 0.3e1) + (-0.143e3 * pow(t, 0.5e1) - 0.304e3 * pow(t, 0.4e1) * u - 0.160e3 * pow(t, 0.3e1) * u * u - 0.28e2 * t * t * pow(u, 0.3e1) + 0.3e1 * t * pow(u, 0.4e1)) * q3 * q3 * q4 * q4 + (0.66e2 * pow(t, 0.6e1) + 0.156e3 * pow(t, 0.5e1) * u + 0.116e3 * pow(t, 0.4e1) * u * u + 0.16e2 * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.10e2 * t * t * pow(u, 0.4e1)) * q3 * q3 * q4 + (-0.42e2 * pow(t, 0.3e1) - 0.44e2 * t * t * u - 0.18e2 * t * u * u) * q3 * pow(q4, 0.5e1) + (0.156e3 * pow(t, 0.4e1) + 0.216e3 * pow(t, 0.3e1) * u + 0.112e3 * t * t * u * u + 0.20e2 * t * pow(u, 0.3e1)) * q3 * pow(q4, 0.4e1) + (-0.224e3 * pow(t, 0.5e1) - 0.429e3 * pow(t, 0.4e1) * u - 0.336e3 * pow(t, 0.3e1) * u * u - 0.142e3 * t * t * pow(u, 0.3e1) - 0.32e2 * t * pow(u, 0.4e1) - 0.5e1 * pow(u, 0.5e1)) * q3 * pow(q4, 0.3e1) + (0.136e3 * pow(t, 0.6e1) + 0.322e3 * pow(t, 0.5e1) * u + 0.336e3 * pow(t, 0.4e1) * u * u + 0.224e3 * pow(t, 0.3e1) * pow(u, 0.3e1) + 0.84e2 * t * t * pow(u, 0.4e1) + 0.10e2 * t * pow(u, 0.5e1)) * q3 * q4 * q4 + (-0.16e2 * pow(t, 0.7e1) - 0.17e2 * pow(t, 0.6e1) * u + 0.6e1 * pow(t, 0.5e1) * u * u + 0.2e1 * pow(t, 0.4e1) * pow(u, 0.3e1) - 0.2e1 * pow(t, 0.3e1) * pow(u, 0.4e1) + 0.3e1 * t * t * pow(u, 0.5e1)) * q3 * q4 + 0.96e2 * pow(q3, 0.4e1) * q4 * q4 * pow(t, 0.3e1) + 0.48e2 * t * pow(q3, 0.4e1) * pow(q4, 0.4e1) - 0.160e3 * t * t * pow(q3, 0.4e1) * pow(q4, 0.3e1) + 0.48e2 * t * q3 * q3 * pow(q4, 0.6e1)) * pow(q4 - t, -0.2e1) / u * pow(t, -0.2e1) * pow(0.4e1 * q3 * q4 - t * t - 0.2e1 * t * u - u * u, -0.2e1));
}
#include <math.h>

double GstarVCoeffsC2e1 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return(-0.16e2 * (0.35e2 * pow(t, 0.8e1) * u + 0.75e2 * pow(t, 0.7e1) * u * u + (0.4e1 * pow(t, 0.7e1) + 0.16e2 * pow(t, 0.6e1) * u + 0.24e2 * pow(t, 0.5e1) * u * u + 0.16e2 * pow(t, 0.4e1) * pow(u, 0.3e1) + 0.4e1 * pow(t, 0.3e1) * pow(u, 0.4e1)) * q3 * q3 + (-0.4e1 * pow(t, 0.8e1) - 0.20e2 * pow(t, 0.7e1) * u - 0.40e2 * pow(t, 0.6e1) * u * u - 0.40e2 * pow(t, 0.5e1) * pow(u, 0.3e1) - 0.20e2 * pow(t, 0.4e1) * pow(u, 0.4e1) - 0.4e1 * pow(t, 0.3e1) * pow(u, 0.5e1)) * q3 + (0.4e1 * pow(t, 0.5e1) + 0.14e2 * pow(t, 0.4e1) * u + 0.18e2 * pow(t, 0.3e1) * u * u + 0.10e2 * t * t * pow(u, 0.3e1) + 0.2e1 * t * pow(u, 0.4e1)) * pow(q4, 0.4e1) + (-0.12e2 * pow(t, 0.6e1) - 0.44e2 * pow(t, 0.5e1) * u - 0.62e2 * pow(t, 0.4e1) * u * u - 0.42e2 * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.14e2 * t * t * pow(u, 0.4e1) - 0.2e1 * t * pow(u, 0.5e1)) * pow(q4, 0.3e1) + (0.19e2 * pow(t, 0.7e1) + 0.80e2 * pow(t, 0.6e1) * u + 0.137e3 * pow(t, 0.5e1) * u * u + 0.122e3 * pow(t, 0.4e1) * pow(u, 0.3e1) + 0.59e2 * pow(t, 0.3e1) * pow(u, 0.4e1) + 0.14e2 * t * t * pow(u, 0.5e1) + t * pow(u, 0.6e1)) * q4 * q4 + (-0.18e2 * pow(t, 0.8e1) - 0.85e2 * pow(t, 0.7e1) * u - 0.169e3 * pow(t, 0.6e1) * u * u - 0.184e3 * pow(t, 0.5e1) * pow(u, 0.3e1) - 0.118e3 * pow(t, 0.4e1) * pow(u, 0.4e1) - 0.43e2 * pow(t, 0.3e1) * pow(u, 0.5e1) - 0.7e1 * t * t * pow(u, 0.6e1)) * q4 + 0.7e1 * pow(t, 0.9e1) + (0.48e2 * t - 0.96e2 * u) * pow(q3, 0.3e1) * pow(q4, 0.5e1) + (-0.136e3 * t * t + 0.200e3 * t * u) * pow(q3, 0.3e1) * pow(q4, 0.4e1) + (0.120e3 * pow(t, 0.3e1) - 0.48e2 * t * t * u - 0.24e2 * t * u * u) * pow(q3, 0.3e1) * pow(q4, 0.3e1) + (0.8e1 * pow(t, 0.4e1) + 0.56e2 * pow(t, 0.3e1) * u + 0.64e2 * t * t * u * u) * pow(q3, 0.3e1) * q4 * q4 + (-0.32e2 * pow(t, 0.5e1) - 0.64e2 * pow(t, 0.4e1) * u - 0.32e2 * pow(t, 0.3e1) * u * u) * pow(q3, 0.3e1) * q4 + (-0.144e3 * t * t - 0.32e2 * t * u) * q3 * q3 * pow(q4, 0.5e1) + (0.220e3 * pow(t, 0.3e1) + 0.176e3 * t * t * u + 0.84e2 * t * u * u + 0.48e2 * pow(u, 0.3e1)) * q3 * q3 * pow(q4, 0.4e1) + (-0.176e3 * pow(t, 0.4e1) - 0.236e3 * pow(t, 0.3e1) * u - 0.240e3 * t * t * u * u - 0.100e3 * t * pow(u, 0.3e1)) * q3 * q3 * pow(q4, 0.3e1) + (0.23e2 * pow(t, 0.5e1) - 0.4e1 * pow(t, 0.4e1) * u + 0.30e2 * pow(t, 0.3e1) * u * u + 0.12e2 * t * t * pow(u, 0.3e1) + 0.3e1 * t * pow(u, 0.4e1)) * q3 * q3 * q4 * q4 + (0.24e2 * pow(t, 0.6e1) + 0.60e2 * pow(t, 0.5e1) * u + 0.48e2 * pow(t, 0.4e1) * u * u + 0.4e1 * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.8e1 * t * t * pow(u, 0.4e1)) * q3 * q3 * q4 + (-0.32e2 * pow(t, 0.3e1) - 0.40e2 * t * t * u - 0.24e2 * t * u * u) * q3 * pow(q4, 0.5e1) + (0.98e2 * pow(t, 0.4e1) + 0.138e3 * pow(t, 0.3e1) * u + 0.94e2 * t * t * u * u + 0.22e2 * t * pow(u, 0.3e1)) * q3 * pow(q4, 0.4e1) + (-0.153e3 * pow(t, 0.5e1) - 0.308e3 * pow(t, 0.4e1) * u - 0.280e3 * pow(t, 0.3e1) * u * u - 0.134e3 * t * t * pow(u, 0.3e1) - 0.31e2 * t * pow(u, 0.4e1) - 0.6e1 * pow(u, 0.5e1)) * q3 * pow(q4, 0.3e1) + (0.137e3 * pow(t, 0.6e1) + 0.359e3 * pow(t, 0.5e1) * u + 0.416e3 * pow(t, 0.4e1) * u * u + 0.284e3 * pow(t, 0.3e1) * pow(u, 0.3e1) + 0.103e3 * t * t * pow(u, 0.4e1) + 0.13e2 * t * pow(u, 0.5e1)) * q3 * q4 * q4 + (-0.46e2 * pow(t, 0.7e1) - 0.127e3 * pow(t, 0.6e1) * u - 0.150e3 * pow(t, 0.5e1) * u * u - 0.104e3 * pow(t, 0.4e1) * pow(u, 0.3e1) - 0.36e2 * pow(t, 0.3e1) * pow(u, 0.4e1) - t * t * pow(u, 0.5e1)) * q3 * q4 + 0.90e2 * pow(t, 0.6e1) * pow(u, 0.3e1) + 0.65e2 * pow(t, 0.5e1) * pow(u, 0.4e1) + 0.27e2 * pow(t, 0.4e1) * pow(u, 0.5e1) + 0.5e1 * pow(t, 0.3e1) * pow(u, 0.6e1) + 0.64e2 * pow(q3, 0.4e1) * q4 * q4 * pow(t, 0.3e1) + 0.48e2 * t * pow(q3, 0.4e1) * pow(q4, 0.4e1) - 0.128e3 * t * t * pow(q3, 0.4e1) * pow(q4, 0.3e1) + 0.48e2 * t * q3 * q3 * pow(q4, 0.6e1)) * pow(q4 - t, -0.2e1) / u * pow(t, -0.2e1) * pow(0.4e1 * q3 * q4 - t * t - 0.2e1 * t * u - u * u, -0.2e1));
}
#include <math.h>

double GstarVCoeffsC2e2 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return(0.16e2 * (0.8e1 * t * pow(q3, 0.3e1) * q4 * q4 + (0.4e1 * t - 0.20e2 * u) * q3 * q3 * pow(q4, 0.3e1) + (0.12e2 * t * t + 0.12e2 * t * u) * q3 * q3 * q4 * q4 + (0.2e1 * pow(t, 0.3e1) - 0.8e1 * t * t * u - 0.2e1 * t * u * u) * q3 * q3 * q4 + 0.8e1 * t * q3 * pow(q4, 0.4e1) + (-0.12e2 * t * t - 0.12e2 * t * u) * q3 * pow(q4, 0.3e1) + (0.25e2 * pow(t, 0.3e1) + 0.15e2 * t * t * u + 0.19e2 * t * u * u + 0.5e1 * pow(u, 0.3e1)) * q3 * q4 * q4 + (-0.26e2 * pow(t, 0.4e1) - 0.30e2 * pow(t, 0.3e1) * u - 0.14e2 * t * t * u * u - 0.2e1 * t * pow(u, 0.3e1)) * q3 * q4 + (-0.4e1 * t * t * u - 0.4e1 * t * u * u) * pow(q4, 0.3e1) + (0.8e1 * pow(t, 0.3e1) * u + 0.12e2 * t * t * u * u + 0.4e1 * t * pow(u, 0.3e1)) * q4 * q4 + (-0.5e1 * pow(t, 0.5e1) - 0.14e2 * pow(t, 0.4e1) * u - 0.16e2 * pow(t, 0.3e1) * u * u - 0.10e2 * t * t * pow(u, 0.3e1) - 0.3e1 * t * pow(u, 0.4e1)) * q4 + 0.5e1 * pow(t, 0.6e1) + 0.15e2 * pow(t, 0.5e1) * u + 0.17e2 * pow(t, 0.4e1) * u * u + 0.9e1 * pow(t, 0.3e1) * pow(u, 0.3e1) + 0.2e1 * t * t * pow(u, 0.4e1)) / (0.4e1 * q3 * q4 - t * t - 0.2e1 * t * u - u * u) * pow(t, -0.2e1) / u / (q4 - t));
}
#include <math.h>

double GstarVCoeffsC3em1 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return(0.64e2 / u / t * q3 * q3 + (-0.64e2 * pow(u, -0.2e1) + 0.128e3 / u / t) * q3 * q4 + (-0.64e2 / u - 0.128e3 / t) * q3 + 0.64e2 / u / t * q4 * q4 + (-0.64e2 / u - 0.128e3 / t) * q4 + 0.32e2 * t / u + 0.64e2 + 0.64e2 * u / t);
}
#include <math.h>

double GstarVCoeffsC3e0 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return(-0.16e2 * (0.9e1 * u * pow(q3, 0.4e1) * q4 * q4 - 0.22e2 * u * u * pow(q3, 0.4e1) * q4 + 0.12e2 * pow(u, 0.3e1) * pow(q3, 0.4e1) + (-0.13e2 * t + 0.14e2 * u) * pow(q3, 0.3e1) * pow(q4, 0.3e1) + (0.20e2 * t * u - 0.68e2 * u * u) * pow(q3, 0.3e1) * q4 * q4 + (0.7e1 * t * u * u + 0.100e3 * pow(u, 0.3e1)) * pow(q3, 0.3e1) * q4 + (-0.12e2 * t * pow(u, 0.3e1) - 0.44e2 * pow(u, 0.4e1)) * pow(q3, 0.3e1) + 0.9e1 * u * q3 * q3 * pow(q4, 0.4e1) + (0.20e2 * t * u - 0.68e2 * u * u) * q3 * q3 * pow(q4, 0.3e1) + (0.5e1 * t * t * u - 0.10e2 * t * u * u + 0.174e3 * pow(u, 0.3e1)) * q3 * q3 * q4 * q4 + (-0.14e2 * t * t * u * u - 0.54e2 * t * pow(u, 0.3e1) - 0.180e3 * pow(u, 0.4e1)) * q3 * q3 * q4 + (0.8e1 * t * t * pow(u, 0.3e1) + 0.40e2 * t * pow(u, 0.4e1) + 0.64e2 * pow(u, 0.5e1)) * q3 * q3 - 0.22e2 * u * u * q3 * pow(q4, 0.4e1) + (0.7e1 * t * u * u + 0.100e3 * pow(u, 0.3e1)) * q3 * pow(q4, 0.3e1) + (-0.14e2 * t * t * u * u - 0.54e2 * t * pow(u, 0.3e1) - 0.180e3 * pow(u, 0.4e1)) * q3 * q4 * q4 + (0.36e2 * t * t * pow(u, 0.3e1) + 0.91e2 * t * pow(u, 0.4e1) + 0.146e3 * pow(u, 0.5e1)) * q3 * q4 + (-0.20e2 * t * t * pow(u, 0.4e1) - 0.42e2 * t * pow(u, 0.5e1) - 0.44e2 * pow(u, 0.6e1)) * q3 + 0.12e2 * pow(u, 0.3e1) * pow(q4, 0.4e1) + (-0.12e2 * t * pow(u, 0.3e1) - 0.44e2 * pow(u, 0.4e1)) * pow(q4, 0.3e1) + (0.8e1 * t * t * pow(u, 0.3e1) + 0.40e2 * t * pow(u, 0.4e1) + 0.64e2 * pow(u, 0.5e1)) * q4 * q4 + (-0.20e2 * t * t * pow(u, 0.4e1) - 0.42e2 * t * pow(u, 0.5e1) - 0.44e2 * pow(u, 0.6e1)) * q4 + 0.11e2 * t * t * pow(u, 0.5e1) + 0.14e2 * t * pow(u, 0.6e1) + 0.12e2 * pow(u, 0.7e1)) * pow(q3 - u, -0.2e1) * pow(u, -0.2e1) / t * pow(q4 - u, -0.2e1));
}
#include <math.h>

double GstarVCoeffsC3e1 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return(0.16e2 * (0.6e1 * u * pow(q3, 0.4e1) * q4 * q4 - 0.15e2 * u * u * pow(q3, 0.4e1) * q4 + 0.8e1 * pow(u, 0.3e1) * pow(q3, 0.4e1) + (-0.15e2 * t + 0.3e1 * u) * pow(q3, 0.3e1) * pow(q4, 0.3e1) + (0.28e2 * t * u - 0.25e2 * u * u) * pow(q3, 0.3e1) * q4 * q4 + (-0.2e1 * t * u * u + 0.48e2 * pow(u, 0.3e1)) * pow(q3, 0.3e1) * q4 + (-0.9e1 * t * pow(u, 0.3e1) - 0.24e2 * pow(u, 0.4e1)) * pow(q3, 0.3e1) + 0.6e1 * u * q3 * q3 * pow(q4, 0.4e1) + (0.28e2 * t * u - 0.25e2 * u * u) * q3 * q3 * pow(q4, 0.3e1) + (0.5e1 * t * t * u - 0.33e2 * t * u * u + 0.66e2 * pow(u, 0.3e1)) * q3 * q3 * q4 * q4 + (-0.16e2 * t * t * u * u - 0.37e2 * t * pow(u, 0.3e1) - 0.86e2 * pow(u, 0.4e1)) * q3 * q3 * q4 + (0.10e2 * t * t * pow(u, 0.3e1) + 0.38e2 * t * pow(u, 0.4e1) + 0.38e2 * pow(u, 0.5e1)) * q3 * q3 - 0.15e2 * u * u * q3 * pow(q4, 0.4e1) + (-0.2e1 * t * u * u + 0.48e2 * pow(u, 0.3e1)) * q3 * pow(q4, 0.3e1) + (-0.16e2 * t * t * u * u - 0.37e2 * t * pow(u, 0.3e1) - 0.86e2 * pow(u, 0.4e1)) * q3 * q4 * q4 + (0.44e2 * t * t * pow(u, 0.3e1) + 0.91e2 * t * pow(u, 0.4e1) + 0.89e2 * pow(u, 0.5e1)) * q3 * q4 + (-0.26e2 * t * t * pow(u, 0.4e1) - 0.50e2 * t * pow(u, 0.5e1) - 0.36e2 * pow(u, 0.6e1)) * q3 + 0.8e1 * pow(u, 0.3e1) * pow(q4, 0.4e1) + (-0.9e1 * t * pow(u, 0.3e1) - 0.24e2 * pow(u, 0.4e1)) * pow(q4, 0.3e1) + (0.10e2 * t * t * pow(u, 0.3e1) + 0.38e2 * t * pow(u, 0.4e1) + 0.38e2 * pow(u, 0.5e1)) * q4 * q4 + (-0.26e2 * t * t * pow(u, 0.4e1) - 0.50e2 * t * pow(u, 0.5e1) - 0.36e2 * pow(u, 0.6e1)) * q4 + 0.15e2 * t * t * pow(u, 0.5e1) + 0.21e2 * t * pow(u, 0.6e1) + 0.14e2 * pow(u, 0.7e1)) * pow(q3 - u, -0.2e1) * pow(u, -0.2e1) / t * pow(q4 - u, -0.2e1));
}
#include <math.h>

double GstarVCoeffsC3e2 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return(-0.16e2 * (u * pow(q3, 0.3e1) * q4 + (-0.7e1 * t - 0.4e1 * u) * q3 * q3 * q4 * q4 + (0.5e1 * t * u + 0.8e1 * u * u) * q3 * q3 * q4 + u * u * t * q3 * q3 + u * q3 * pow(q4, 0.3e1) + (0.5e1 * t * u + 0.8e1 * u * u) * q3 * q4 * q4 + (0.3e1 * t * t * u - t * u * u - 0.4e1 * pow(u, 0.3e1)) * q3 * q4 + (-0.3e1 * t * t * u * u - 0.8e1 * t * pow(u, 0.3e1) - 0.10e2 * pow(u, 0.4e1)) * q3 + u * u * t * q4 * q4 + (-0.3e1 * t * t * u * u - 0.8e1 * t * pow(u, 0.3e1) - 0.10e2 * pow(u, 0.4e1)) * q4 + 0.3e1 * t * t * pow(u, 0.3e1) + 0.12e2 * t * pow(u, 0.4e1) + 0.10e2 * pow(u, 0.5e1)) / (q4 - u) / t * pow(u, -0.2e1) / (q3 - u));
}
#include <math.h>

double GstarVCoeffsC4em1 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return(0.64e2 / u / t * q3 * q3 + (0.128e3 / u / t - 0.64e2 * pow(t, -0.2e1)) * q3 * q4 + (-0.128e3 / u - 0.64e2 / t) * q3 + 0.64e2 / u / t * q4 * q4 + (-0.128e3 / u - 0.64e2 / t) * q4 + 0.64e2 * t / u + 0.64e2 + 0.32e2 * u / t);
}
#include <math.h>

double GstarVCoeffsC4e0 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return(-0.16e2 * (0.9e1 * t * pow(q3, 0.4e1) * q4 * q4 - 0.22e2 * t * t * pow(q3, 0.4e1) * q4 + 0.12e2 * pow(t, 0.3e1) * pow(q3, 0.4e1) + (0.14e2 * t - 0.13e2 * u) * pow(q3, 0.3e1) * pow(q4, 0.3e1) + (-0.68e2 * t * t + 0.20e2 * t * u) * pow(q3, 0.3e1) * q4 * q4 + (0.100e3 * pow(t, 0.3e1) + 0.7e1 * t * t * u) * pow(q3, 0.3e1) * q4 + (-0.44e2 * pow(t, 0.4e1) - 0.12e2 * pow(t, 0.3e1) * u) * pow(q3, 0.3e1) + 0.9e1 * t * q3 * q3 * pow(q4, 0.4e1) + (-0.68e2 * t * t + 0.20e2 * t * u) * q3 * q3 * pow(q4, 0.3e1) + (0.174e3 * pow(t, 0.3e1) - 0.10e2 * t * t * u + 0.5e1 * t * u * u) * q3 * q3 * q4 * q4 + (-0.180e3 * pow(t, 0.4e1) - 0.54e2 * pow(t, 0.3e1) * u - 0.14e2 * t * t * u * u) * q3 * q3 * q4 + (0.64e2 * pow(t, 0.5e1) + 0.40e2 * pow(t, 0.4e1) * u + 0.8e1 * pow(t, 0.3e1) * u * u) * q3 * q3 - 0.22e2 * t * t * q3 * pow(q4, 0.4e1) + (0.100e3 * pow(t, 0.3e1) + 0.7e1 * t * t * u) * q3 * pow(q4, 0.3e1) + (-0.180e3 * pow(t, 0.4e1) - 0.54e2 * pow(t, 0.3e1) * u - 0.14e2 * t * t * u * u) * q3 * q4 * q4 + (0.146e3 * pow(t, 0.5e1) + 0.91e2 * pow(t, 0.4e1) * u + 0.36e2 * pow(t, 0.3e1) * u * u) * q3 * q4 + (-0.44e2 * pow(t, 0.6e1) - 0.42e2 * pow(t, 0.5e1) * u - 0.20e2 * pow(t, 0.4e1) * u * u) * q3 + 0.12e2 * pow(t, 0.3e1) * pow(q4, 0.4e1) + (-0.44e2 * pow(t, 0.4e1) - 0.12e2 * pow(t, 0.3e1) * u) * pow(q4, 0.3e1) + (0.64e2 * pow(t, 0.5e1) + 0.40e2 * pow(t, 0.4e1) * u + 0.8e1 * pow(t, 0.3e1) * u * u) * q4 * q4 + (-0.44e2 * pow(t, 0.6e1) - 0.42e2 * pow(t, 0.5e1) * u - 0.20e2 * pow(t, 0.4e1) * u * u) * q4 + 0.12e2 * pow(t, 0.7e1) + 0.14e2 * pow(t, 0.6e1) * u + 0.11e2 * pow(t, 0.5e1) * u * u) * pow(t, -0.2e1) / u * pow(q4 - t, -0.2e1) * pow(q3 - t, -0.2e1));
}
#include <math.h>

double GstarVCoeffsC4e1 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return(0.16e2 * (0.6e1 * t * pow(q3, 0.4e1) * q4 * q4 - 0.15e2 * t * t * pow(q3, 0.4e1) * q4 + 0.8e1 * pow(t, 0.3e1) * pow(q3, 0.4e1) + (0.3e1 * t - 0.15e2 * u) * pow(q3, 0.3e1) * pow(q4, 0.3e1) + (-0.25e2 * t * t + 0.28e2 * t * u) * pow(q3, 0.3e1) * q4 * q4 + (0.48e2 * pow(t, 0.3e1) - 0.2e1 * t * t * u) * pow(q3, 0.3e1) * q4 + (-0.24e2 * pow(t, 0.4e1) - 0.9e1 * pow(t, 0.3e1) * u) * pow(q3, 0.3e1) + 0.6e1 * t * q3 * q3 * pow(q4, 0.4e1) + (-0.25e2 * t * t + 0.28e2 * t * u) * q3 * q3 * pow(q4, 0.3e1) + (0.66e2 * pow(t, 0.3e1) - 0.33e2 * t * t * u + 0.5e1 * t * u * u) * q3 * q3 * q4 * q4 + (-0.86e2 * pow(t, 0.4e1) - 0.37e2 * pow(t, 0.3e1) * u - 0.16e2 * t * t * u * u) * q3 * q3 * q4 + (0.38e2 * pow(t, 0.5e1) + 0.38e2 * pow(t, 0.4e1) * u + 0.10e2 * pow(t, 0.3e1) * u * u) * q3 * q3 - 0.15e2 * t * t * q3 * pow(q4, 0.4e1) + (0.48e2 * pow(t, 0.3e1) - 0.2e1 * t * t * u) * q3 * pow(q4, 0.3e1) + (-0.86e2 * pow(t, 0.4e1) - 0.37e2 * pow(t, 0.3e1) * u - 0.16e2 * t * t * u * u) * q3 * q4 * q4 + (0.89e2 * pow(t, 0.5e1) + 0.91e2 * pow(t, 0.4e1) * u + 0.44e2 * pow(t, 0.3e1) * u * u) * q3 * q4 + (-0.36e2 * pow(t, 0.6e1) - 0.50e2 * pow(t, 0.5e1) * u - 0.26e2 * pow(t, 0.4e1) * u * u) * q3 + 0.8e1 * pow(t, 0.3e1) * pow(q4, 0.4e1) + (-0.24e2 * pow(t, 0.4e1) - 0.9e1 * pow(t, 0.3e1) * u) * pow(q4, 0.3e1) + (0.38e2 * pow(t, 0.5e1) + 0.38e2 * pow(t, 0.4e1) * u + 0.10e2 * pow(t, 0.3e1) * u * u) * q4 * q4 + (-0.36e2 * pow(t, 0.6e1) - 0.50e2 * pow(t, 0.5e1) * u - 0.26e2 * pow(t, 0.4e1) * u * u) * q4 + 0.14e2 * pow(t, 0.7e1) + 0.21e2 * pow(t, 0.6e1) * u + 0.15e2 * pow(t, 0.5e1) * u * u) * pow(t, -0.2e1) / u * pow(q4 - t, -0.2e1) * pow(q3 - t, -0.2e1));
}
#include <math.h>

double GstarVCoeffsC4e2 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return(-0.16e2 * (t * pow(q3, 0.3e1) * q4 + (-0.4e1 * t - 0.7e1 * u) * q3 * q3 * q4 * q4 + (0.8e1 * t * t + 0.5e1 * t * u) * q3 * q3 * q4 + u * t * t * q3 * q3 + t * q3 * pow(q4, 0.3e1) + (0.8e1 * t * t + 0.5e1 * t * u) * q3 * q4 * q4 + (-0.4e1 * pow(t, 0.3e1) - t * t * u + 0.3e1 * t * u * u) * q3 * q4 + (-0.10e2 * pow(t, 0.4e1) - 0.8e1 * pow(t, 0.3e1) * u - 0.3e1 * t * t * u * u) * q3 + u * t * t * q4 * q4 + (-0.10e2 * pow(t, 0.4e1) - 0.8e1 * pow(t, 0.3e1) * u - 0.3e1 * t * t * u * u) * q4 + 0.10e2 * pow(t, 0.5e1) + 0.12e2 * pow(t, 0.4e1) * u + 0.3e1 * pow(t, 0.3e1) * u * u) / (q3 - t) / (q4 - t) / u * pow(t, -0.2e1));
}
#include <math.h>

double GstarVCoeffsC5em1 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return(-0.32e2 / u / t * q3 * q3 + (0.32e2 * pow(u, -0.2e1) - 0.64e2 / u / t) * q3 * q4 + (0.32e2 / u + 0.64e2 / t) * q3 - 0.32e2 / u / t * q4 * q4 + (0.32e2 / u + 0.64e2 / t) * q4 - 0.16e2 * t / u - 0.32e2 - 0.32e2 * u / t);
}
#include <math.h>

double GstarVCoeffsC5e0 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return(0.16e2 * ((-0.24e2 * t * t * u + 0.40e2 * t * u * u + 0.336e3 * pow(u, 0.3e1)) * pow(q3, 0.3e1) * pow(q4, 0.3e1) + (0.3e1 * pow(t, 0.4e1) * u - 0.28e2 * pow(t, 0.3e1) * u * u - 0.160e3 * t * t * pow(u, 0.3e1) - 0.304e3 * t * pow(u, 0.4e1) - 0.143e3 * pow(u, 0.5e1)) * q3 * q3 * q4 * q4 + (0.3e1 * pow(t, 0.5e1) * u * u - 0.2e1 * pow(t, 0.4e1) * pow(u, 0.3e1) + 0.2e1 * pow(t, 0.3e1) * pow(u, 0.4e1) + 0.6e1 * t * t * pow(u, 0.5e1) - 0.17e2 * t * pow(u, 0.6e1) - 0.16e2 * pow(u, 0.7e1)) * q3 * q4 + 0.48e2 * pow(q3, 0.4e1) * pow(q4, 0.4e1) * u + 0.6e1 * pow(u, 0.9e1) + (0.6e1 * pow(t, 0.4e1) * pow(u, 0.3e1) + 0.24e2 * pow(t, 0.3e1) * pow(u, 0.4e1) + 0.36e2 * t * t * pow(u, 0.5e1) + 0.24e2 * t * pow(u, 0.6e1) + 0.6e1 * pow(u, 0.7e1)) * q3 * q3 + (-0.6e1 * pow(t, 0.5e1) * pow(u, 0.3e1) - 0.34e2 * pow(t, 0.4e1) * pow(u, 0.4e1) - 0.76e2 * pow(t, 0.3e1) * pow(u, 0.5e1) - 0.84e2 * t * t * pow(u, 0.6e1) - 0.46e2 * t * pow(u, 0.7e1) - 0.10e2 * pow(u, 0.8e1)) * q3 + (0.8e1 * pow(t, 0.3e1) * u * u + 0.22e2 * t * t * pow(u, 0.3e1) + 0.20e2 * t * pow(u, 0.4e1) + 0.6e1 * pow(u, 0.5e1)) * pow(q4, 0.4e1) + (-0.10e2 * pow(t, 0.4e1) * u * u - 0.56e2 * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.104e3 * t * t * pow(u, 0.4e1) - 0.80e2 * t * pow(u, 0.5e1) - 0.22e2 * pow(u, 0.6e1)) * pow(q4, 0.3e1) + (pow(t, 0.6e1) * u + 0.12e2 * pow(t, 0.5e1) * u * u + 0.64e2 * pow(t, 0.4e1) * pow(u, 0.3e1) + 0.164e3 * pow(t, 0.3e1) * pow(u, 0.4e1) + 0.211e3 * t * t * pow(u, 0.5e1) + 0.132e3 * t * pow(u, 0.6e1) + 0.32e2 * pow(u, 0.7e1)) * q4 * q4 + (-0.6e1 * pow(t, 0.6e1) * u * u - 0.38e2 * pow(t, 0.5e1) * pow(u, 0.3e1) - 0.114e3 * pow(t, 0.4e1) * pow(u, 0.4e1) - 0.196e3 * pow(t, 0.3e1) * pow(u, 0.5e1) - 0.194e3 * t * t * pow(u, 0.6e1) - 0.102e3 * t * pow(u, 0.7e1) - 0.22e2 * pow(u, 0.8e1)) * q4 + 0.64e2 * t * t * pow(u, 0.7e1) + 0.30e2 * t * pow(u, 0.8e1) + 0.4e1 * pow(t, 0.6e1) * pow(u, 0.3e1) + 0.22e2 * pow(t, 0.5e1) * pow(u, 0.4e1) + 0.54e2 * pow(t, 0.4e1) * pow(u, 0.5e1) + 0.76e2 * pow(t, 0.3e1) * pow(u, 0.6e1) + (-0.80e2 * t + 0.64e2 * u) * pow(q3, 0.3e1) * pow(q4, 0.5e1) + (0.140e3 * t * u - 0.292e3 * u * u) * pow(q3, 0.3e1) * pow(q4, 0.4e1) + (0.80e2 * t * t * u * u + 0.44e2 * t * pow(u, 0.3e1) - 0.52e2 * pow(u, 0.4e1)) * pow(q3, 0.3e1) * q4 * q4 + (-0.48e2 * t * t * pow(u, 0.3e1) - 0.96e2 * t * pow(u, 0.4e1) - 0.48e2 * pow(u, 0.5e1)) * pow(q3, 0.3e1) * q4 + (-0.44e2 * t * u - 0.188e3 * u * u) * q3 * q3 * pow(q4, 0.5e1) + (0.40e2 * pow(t, 0.3e1) + 0.78e2 * t * t * u + 0.196e3 * t * u * u + 0.254e3 * pow(u, 0.3e1)) * q3 * q3 * pow(q4, 0.4e1) + (-0.72e2 * pow(t, 0.3e1) * u - 0.124e3 * t * t * u * u - 0.48e2 * t * pow(u, 0.3e1) - 0.44e2 * pow(u, 0.4e1)) * q3 * q3 * pow(q4, 0.3e1) + (-0.10e2 * pow(t, 0.4e1) * u * u + 0.16e2 * pow(t, 0.3e1) * pow(u, 0.3e1) + 0.116e3 * t * t * pow(u, 0.4e1) + 0.156e3 * t * pow(u, 0.5e1) + 0.66e2 * pow(u, 0.6e1)) * q3 * q3 * q4 + (-0.18e2 * t * t * u - 0.44e2 * t * u * u - 0.42e2 * pow(u, 0.3e1)) * q3 * pow(q4, 0.5e1) + (0.20e2 * pow(t, 0.3e1) * u + 0.112e3 * t * t * u * u + 0.216e3 * t * pow(u, 0.3e1) + 0.156e3 * pow(u, 0.4e1)) * q3 * pow(q4, 0.4e1) + (-0.5e1 * pow(t, 0.5e1) - 0.32e2 * pow(t, 0.4e1) * u - 0.142e3 * pow(t, 0.3e1) * u * u - 0.336e3 * t * t * pow(u, 0.3e1) - 0.429e3 * t * pow(u, 0.4e1) - 0.224e3 * pow(u, 0.5e1)) * q3 * pow(q4, 0.3e1) + (0.10e2 * pow(t, 0.5e1) * u + 0.84e2 * pow(t, 0.4e1) * u * u + 0.224e3 * pow(t, 0.3e1) * pow(u, 0.3e1) + 0.336e3 * t * t * pow(u, 0.4e1) + 0.322e3 * t * pow(u, 0.5e1) + 0.136e3 * pow(u, 0.6e1)) * q3 * q4 * q4 - 0.160e3 * pow(q3, 0.4e1) * pow(q4, 0.3e1) * u * u + 0.96e2 * pow(q3, 0.4e1) * q4 * q4 * pow(u, 0.3e1) + 0.48e2 * u * q3 * q3 * pow(q4, 0.6e1)) / t * pow(u, -0.2e1) * pow(q4 - u, -0.2e1) * pow(0.4e1 * q3 * q4 - t * t - 0.2e1 * t * u - u * u, -0.2e1));
}
#include <math.h>

double GstarVCoeffsC5e1 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return(-0.16e2 * (0.48e2 * pow(q3, 0.4e1) * pow(q4, 0.4e1) * u + (-0.96e2 * t + 0.48e2 * u) * pow(q3, 0.3e1) * pow(q4, 0.5e1) + (0.200e3 * t * u - 0.136e3 * u * u) * pow(q3, 0.3e1) * pow(q4, 0.4e1) + (0.64e2 * t * t * u * u + 0.56e2 * t * pow(u, 0.3e1) + 0.8e1 * pow(u, 0.4e1)) * pow(q3, 0.3e1) * q4 * q4 + (-0.32e2 * t * t * pow(u, 0.3e1) - 0.64e2 * t * pow(u, 0.4e1) - 0.32e2 * pow(u, 0.5e1)) * pow(q3, 0.3e1) * q4 + (-0.32e2 * t * u - 0.144e3 * u * u) * q3 * q3 * pow(q4, 0.5e1) + (0.48e2 * pow(t, 0.3e1) + 0.84e2 * t * t * u + 0.176e3 * t * u * u + 0.220e3 * pow(u, 0.3e1)) * q3 * q3 * pow(q4, 0.4e1) + (-0.100e3 * pow(t, 0.3e1) * u - 0.240e3 * t * t * u * u - 0.236e3 * t * pow(u, 0.3e1) - 0.176e3 * pow(u, 0.4e1)) * q3 * q3 * pow(q4, 0.3e1) + (-0.8e1 * pow(t, 0.4e1) * u * u + 0.4e1 * pow(t, 0.3e1) * pow(u, 0.3e1) + 0.48e2 * t * t * pow(u, 0.4e1) + 0.60e2 * t * pow(u, 0.5e1) + 0.24e2 * pow(u, 0.6e1)) * q3 * q3 * q4 + (-0.24e2 * t * t * u - 0.40e2 * t * u * u - 0.32e2 * pow(u, 0.3e1)) * q3 * pow(q4, 0.5e1) + (0.22e2 * pow(t, 0.3e1) * u + 0.94e2 * t * t * u * u + 0.138e3 * t * pow(u, 0.3e1) + 0.98e2 * pow(u, 0.4e1)) * q3 * pow(q4, 0.4e1) + (-0.6e1 * pow(t, 0.5e1) - 0.31e2 * pow(t, 0.4e1) * u - 0.134e3 * pow(t, 0.3e1) * u * u - 0.280e3 * t * t * pow(u, 0.3e1) - 0.308e3 * t * pow(u, 0.4e1) - 0.153e3 * pow(u, 0.5e1)) * q3 * pow(q4, 0.3e1) + (0.13e2 * pow(t, 0.5e1) * u + 0.103e3 * pow(t, 0.4e1) * u * u + 0.284e3 * pow(t, 0.3e1) * pow(u, 0.3e1) + 0.416e3 * t * t * pow(u, 0.4e1) + 0.359e3 * t * pow(u, 0.5e1) + 0.137e3 * pow(u, 0.6e1)) * q3 * q4 * q4 + 0.7e1 * pow(u, 0.9e1) + (-0.24e2 * t * t * u - 0.48e2 * t * u * u + 0.120e3 * pow(u, 0.3e1)) * pow(q3, 0.3e1) * pow(q4, 0.3e1) + (0.3e1 * pow(t, 0.4e1) * u + 0.12e2 * pow(t, 0.3e1) * u * u + 0.30e2 * t * t * pow(u, 0.3e1) - 0.4e1 * t * pow(u, 0.4e1) + 0.23e2 * pow(u, 0.5e1)) * q3 * q3 * q4 * q4 + (-pow(t, 0.5e1) * u * u - 0.36e2 * pow(t, 0.4e1) * pow(u, 0.3e1) - 0.104e3 * pow(t, 0.3e1) * pow(u, 0.4e1) - 0.150e3 * t * t * pow(u, 0.5e1) - 0.127e3 * t * pow(u, 0.6e1) - 0.46e2 * pow(u, 0.7e1)) * q3 * q4 + 0.75e2 * t * t * pow(u, 0.7e1) + 0.35e2 * t * pow(u, 0.8e1) + 0.5e1 * pow(t, 0.6e1) * pow(u, 0.3e1) + 0.27e2 * pow(t, 0.5e1) * pow(u, 0.4e1) + 0.65e2 * pow(t, 0.4e1) * pow(u, 0.5e1) + 0.90e2 * pow(t, 0.3e1) * pow(u, 0.6e1) - 0.128e3 * pow(q3, 0.4e1) * pow(q4, 0.3e1) * u * u + 0.64e2 * pow(q3, 0.4e1) * q4 * q4 * pow(u, 0.3e1) + 0.48e2 * u * q3 * q3 * pow(q4, 0.6e1) + (0.4e1 * pow(t, 0.4e1) * pow(u, 0.3e1) + 0.16e2 * pow(t, 0.3e1) * pow(u, 0.4e1) + 0.24e2 * t * t * pow(u, 0.5e1) + 0.16e2 * t * pow(u, 0.6e1) + 0.4e1 * pow(u, 0.7e1)) * q3 * q3 + (-0.4e1 * pow(t, 0.5e1) * pow(u, 0.3e1) - 0.20e2 * pow(t, 0.4e1) * pow(u, 0.4e1) - 0.40e2 * pow(t, 0.3e1) * pow(u, 0.5e1) - 0.40e2 * t * t * pow(u, 0.6e1) - 0.20e2 * t * pow(u, 0.7e1) - 0.4e1 * pow(u, 0.8e1)) * q3 + (0.2e1 * pow(t, 0.4e1) * u + 0.10e2 * pow(t, 0.3e1) * u * u + 0.18e2 * t * t * pow(u, 0.3e1) + 0.14e2 * t * pow(u, 0.4e1) + 0.4e1 * pow(u, 0.5e1)) * pow(q4, 0.4e1) + (-0.2e1 * pow(t, 0.5e1) * u - 0.14e2 * pow(t, 0.4e1) * u * u - 0.42e2 * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.62e2 * t * t * pow(u, 0.4e1) - 0.44e2 * t * pow(u, 0.5e1) - 0.12e2 * pow(u, 0.6e1)) * pow(q4, 0.3e1) + (pow(t, 0.6e1) * u + 0.14e2 * pow(t, 0.5e1) * u * u + 0.59e2 * pow(t, 0.4e1) * pow(u, 0.3e1) + 0.122e3 * pow(t, 0.3e1) * pow(u, 0.4e1) + 0.137e3 * t * t * pow(u, 0.5e1) + 0.80e2 * t * pow(u, 0.6e1) + 0.19e2 * pow(u, 0.7e1)) * q4 * q4 + (-0.7e1 * pow(t, 0.6e1) * u * u - 0.43e2 * pow(t, 0.5e1) * pow(u, 0.3e1) - 0.118e3 * pow(t, 0.4e1) * pow(u, 0.4e1) - 0.184e3 * pow(t, 0.3e1) * pow(u, 0.5e1) - 0.169e3 * t * t * pow(u, 0.6e1) - 0.85e2 * t * pow(u, 0.7e1) - 0.18e2 * pow(u, 0.8e1)) * q4) / t * pow(u, -0.2e1) * pow(q4 - u, -0.2e1) * pow(0.4e1 * q3 * q4 - t * t - 0.2e1 * t * u - u * u, -0.2e1));
}
#include <math.h>

double GstarVCoeffsC5e2 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return(0.16e2 * (0.8e1 * u * pow(q3, 0.3e1) * q4 * q4 + (-0.20e2 * t + 0.4e1 * u) * q3 * q3 * pow(q4, 0.3e1) + (0.12e2 * t * u + 0.12e2 * u * u) * q3 * q3 * q4 * q4 + (-0.2e1 * t * t * u - 0.8e1 * t * u * u + 0.2e1 * pow(u, 0.3e1)) * q3 * q3 * q4 + 0.8e1 * u * q3 * pow(q4, 0.4e1) + (-0.12e2 * t * u - 0.12e2 * u * u) * q3 * pow(q4, 0.3e1) + (0.5e1 * pow(t, 0.3e1) + 0.19e2 * t * t * u + 0.15e2 * t * u * u + 0.25e2 * pow(u, 0.3e1)) * q3 * q4 * q4 + (-0.2e1 * pow(t, 0.3e1) * u - 0.14e2 * t * t * u * u - 0.30e2 * t * pow(u, 0.3e1) - 0.26e2 * pow(u, 0.4e1)) * q3 * q4 + (-0.4e1 * t * t * u - 0.4e1 * t * u * u) * pow(q4, 0.3e1) + (0.4e1 * pow(t, 0.3e1) * u + 0.12e2 * t * t * u * u + 0.8e1 * t * pow(u, 0.3e1)) * q4 * q4 + (-0.3e1 * pow(t, 0.4e1) * u - 0.10e2 * pow(t, 0.3e1) * u * u - 0.16e2 * t * t * pow(u, 0.3e1) - 0.14e2 * t * pow(u, 0.4e1) - 0.5e1 * pow(u, 0.5e1)) * q4 + 0.2e1 * pow(t, 0.4e1) * u * u + 0.9e1 * pow(t, 0.3e1) * pow(u, 0.3e1) + 0.17e2 * t * t * pow(u, 0.4e1) + 0.15e2 * t * pow(u, 0.5e1) + 0.5e1 * pow(u, 0.6e1)) / (0.4e1 * q3 * q4 - t * t - 0.2e1 * t * u - u * u) / (q4 - u) / t * pow(u, -0.2e1));
}
#include <math.h>

double GstarVCoeffsC6em1 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return(-0.32e2 / t / u * q3 * q3 + (-0.64e2 / t / u + 0.32e2 * pow(t, -0.2e1)) * q3 * q4 + (0.64e2 / u + 0.32e2 / t) * q3 - 0.32e2 / t / u * q4 * q4 + (0.64e2 / u + 0.32e2 / t) * q4 - 0.32e2 * t / u - 0.32e2 - 0.16e2 / t * u);
}
#include <math.h>

double GstarVCoeffsC6e0 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return(0.16e2 * (0.48e2 * t * pow(q3, 0.6e1) * q4 * q4 + 0.48e2 * t * pow(q3, 0.4e1) * pow(q4, 0.4e1) - 0.160e3 * t * t * pow(q3, 0.3e1) * pow(q4, 0.4e1) + 0.96e2 * pow(t, 0.3e1) * q3 * q3 * pow(q4, 0.4e1) + (-0.48e2 * pow(t, 0.5e1) - 0.96e2 * pow(t, 0.4e1) * u - 0.48e2 * pow(t, 0.3e1) * u * u) * q3 * pow(q4, 0.3e1) + (0.66e2 * pow(t, 0.6e1) + 0.156e3 * pow(t, 0.5e1) * u + 0.116e3 * pow(t, 0.4e1) * u * u + 0.16e2 * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.10e2 * t * t * pow(u, 0.4e1)) * q3 * q4 * q4 + (-0.16e2 * pow(t, 0.7e1) - 0.17e2 * pow(t, 0.6e1) * u + 0.6e1 * pow(t, 0.5e1) * u * u + 0.2e1 * pow(t, 0.4e1) * pow(u, 0.3e1) - 0.2e1 * pow(t, 0.3e1) * pow(u, 0.4e1) + 0.3e1 * t * t * pow(u, 0.5e1)) * q3 * q4 + (0.64e2 * t - 0.80e2 * u) * pow(q3, 0.5e1) * pow(q4, 0.3e1) + (-0.188e3 * t * t - 0.44e2 * t * u) * pow(q3, 0.5e1) * q4 * q4 + (-0.42e2 * pow(t, 0.3e1) - 0.44e2 * t * t * u - 0.18e2 * t * u * u) * pow(q3, 0.5e1) * q4 + (-0.292e3 * t * t + 0.140e3 * t * u) * pow(q3, 0.4e1) * pow(q4, 0.3e1) + (0.254e3 * pow(t, 0.3e1) + 0.196e3 * t * t * u + 0.78e2 * t * u * u + 0.40e2 * pow(u, 0.3e1)) * pow(q3, 0.4e1) * q4 * q4 + (0.156e3 * pow(t, 0.4e1) + 0.216e3 * pow(t, 0.3e1) * u + 0.112e3 * t * t * u * u + 0.20e2 * t * pow(u, 0.3e1)) * pow(q3, 0.4e1) * q4 + (0.336e3 * pow(t, 0.3e1) + 0.40e2 * t * t * u - 0.24e2 * t * u * u) * pow(q3, 0.3e1) * pow(q4, 0.3e1) + (-0.44e2 * pow(t, 0.4e1) - 0.48e2 * pow(t, 0.3e1) * u - 0.124e3 * t * t * u * u - 0.72e2 * t * pow(u, 0.3e1)) * pow(q3, 0.3e1) * q4 * q4 + (-0.224e3 * pow(t, 0.5e1) - 0.429e3 * pow(t, 0.4e1) * u - 0.336e3 * pow(t, 0.3e1) * u * u - 0.142e3 * t * t * pow(u, 0.3e1) - 0.32e2 * t * pow(u, 0.4e1) - 0.5e1 * pow(u, 0.5e1)) * pow(q3, 0.3e1) * q4 + (-0.52e2 * pow(t, 0.4e1) + 0.44e2 * pow(t, 0.3e1) * u + 0.80e2 * t * t * u * u) * q3 * q3 * pow(q4, 0.3e1) + (-0.143e3 * pow(t, 0.5e1) - 0.304e3 * pow(t, 0.4e1) * u - 0.160e3 * pow(t, 0.3e1) * u * u - 0.28e2 * t * t * pow(u, 0.3e1) + 0.3e1 * t * pow(u, 0.4e1)) * q3 * q3 * q4 * q4 + (0.136e3 * pow(t, 0.6e1) + 0.322e3 * pow(t, 0.5e1) * u + 0.336e3 * pow(t, 0.4e1) * u * u + 0.224e3 * pow(t, 0.3e1) * pow(u, 0.3e1) + 0.84e2 * t * t * pow(u, 0.4e1) + 0.10e2 * t * pow(u, 0.5e1)) * q3 * q3 * q4 + (-0.22e2 * pow(t, 0.6e1) - 0.80e2 * pow(t, 0.5e1) * u - 0.104e3 * pow(t, 0.4e1) * u * u - 0.56e2 * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.10e2 * t * t * pow(u, 0.4e1)) * pow(q3, 0.3e1) + (0.6e1 * pow(t, 0.5e1) + 0.20e2 * pow(t, 0.4e1) * u + 0.22e2 * pow(t, 0.3e1) * u * u + 0.8e1 * t * t * pow(u, 0.3e1)) * pow(q3, 0.4e1) + (0.32e2 * pow(t, 0.7e1) + 0.132e3 * pow(t, 0.6e1) * u + 0.211e3 * pow(t, 0.5e1) * u * u + 0.164e3 * pow(t, 0.4e1) * pow(u, 0.3e1) + 0.64e2 * pow(t, 0.3e1) * pow(u, 0.4e1) + 0.12e2 * t * t * pow(u, 0.5e1) + t * pow(u, 0.6e1)) * q3 * q3 + (-0.22e2 * pow(t, 0.8e1) - 0.102e3 * pow(t, 0.7e1) * u - 0.194e3 * pow(t, 0.6e1) * u * u - 0.196e3 * pow(t, 0.5e1) * pow(u, 0.3e1) - 0.114e3 * pow(t, 0.4e1) * pow(u, 0.4e1) - 0.38e2 * pow(t, 0.3e1) * pow(u, 0.5e1) - 0.6e1 * t * t * pow(u, 0.6e1)) * q3 + (0.6e1 * pow(t, 0.7e1) + 0.24e2 * pow(t, 0.6e1) * u + 0.36e2 * pow(t, 0.5e1) * u * u + 0.24e2 * pow(t, 0.4e1) * pow(u, 0.3e1) + 0.6e1 * pow(t, 0.3e1) * pow(u, 0.4e1)) * q4 * q4 + (-0.10e2 * pow(t, 0.8e1) - 0.46e2 * pow(t, 0.7e1) * u - 0.84e2 * pow(t, 0.6e1) * u * u - 0.76e2 * pow(t, 0.5e1) * pow(u, 0.3e1) - 0.34e2 * pow(t, 0.4e1) * pow(u, 0.4e1) - 0.6e1 * pow(t, 0.3e1) * pow(u, 0.5e1)) * q4 + 0.30e2 * pow(t, 0.8e1) * u + 0.64e2 * pow(t, 0.7e1) * u * u + 0.6e1 * pow(t, 0.9e1) + 0.76e2 * pow(t, 0.6e1) * pow(u, 0.3e1) + 0.54e2 * pow(t, 0.5e1) * pow(u, 0.4e1) + 0.22e2 * pow(t, 0.4e1) * pow(u, 0.5e1) + 0.4e1 * pow(t, 0.3e1) * pow(u, 0.6e1)) * pow(q3 - t, -0.2e1) / u * pow(t, -0.2e1) * pow(0.4e1 * q3 * q4 - t * t - 0.2e1 * t * u - u * u, -0.2e1));
}
#include <math.h>

double GstarVCoeffsC6e1 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return(-0.16e2 * (0.48e2 * t * pow(q3, 0.6e1) * q4 * q4 + 0.48e2 * t * pow(q3, 0.4e1) * pow(q4, 0.4e1) - 0.128e3 * t * t * pow(q3, 0.3e1) * pow(q4, 0.4e1) + 0.64e2 * pow(t, 0.3e1) * q3 * q3 * pow(q4, 0.4e1) + 0.35e2 * pow(t, 0.8e1) * u + 0.75e2 * pow(t, 0.7e1) * u * u + 0.7e1 * pow(t, 0.9e1) + (0.4e1 * pow(t, 0.5e1) + 0.14e2 * pow(t, 0.4e1) * u + 0.18e2 * pow(t, 0.3e1) * u * u + 0.10e2 * t * t * pow(u, 0.3e1) + 0.2e1 * t * pow(u, 0.4e1)) * pow(q3, 0.4e1) + (-0.12e2 * pow(t, 0.6e1) - 0.44e2 * pow(t, 0.5e1) * u - 0.62e2 * pow(t, 0.4e1) * u * u - 0.42e2 * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.14e2 * t * t * pow(u, 0.4e1) - 0.2e1 * t * pow(u, 0.5e1)) * pow(q3, 0.3e1) + (0.19e2 * pow(t, 0.7e1) + 0.80e2 * pow(t, 0.6e1) * u + 0.137e3 * pow(t, 0.5e1) * u * u + 0.122e3 * pow(t, 0.4e1) * pow(u, 0.3e1) + 0.59e2 * pow(t, 0.3e1) * pow(u, 0.4e1) + 0.14e2 * t * t * pow(u, 0.5e1) + t * pow(u, 0.6e1)) * q3 * q3 + (-0.18e2 * pow(t, 0.8e1) - 0.85e2 * pow(t, 0.7e1) * u - 0.169e3 * pow(t, 0.6e1) * u * u - 0.184e3 * pow(t, 0.5e1) * pow(u, 0.3e1) - 0.118e3 * pow(t, 0.4e1) * pow(u, 0.4e1) - 0.43e2 * pow(t, 0.3e1) * pow(u, 0.5e1) - 0.7e1 * t * t * pow(u, 0.6e1)) * q3 + (0.4e1 * pow(t, 0.7e1) + 0.16e2 * pow(t, 0.6e1) * u + 0.24e2 * pow(t, 0.5e1) * u * u + 0.16e2 * pow(t, 0.4e1) * pow(u, 0.3e1) + 0.4e1 * pow(t, 0.3e1) * pow(u, 0.4e1)) * q4 * q4 + (-0.4e1 * pow(t, 0.8e1) - 0.20e2 * pow(t, 0.7e1) * u - 0.40e2 * pow(t, 0.6e1) * u * u - 0.40e2 * pow(t, 0.5e1) * pow(u, 0.3e1) - 0.20e2 * pow(t, 0.4e1) * pow(u, 0.4e1) - 0.4e1 * pow(t, 0.3e1) * pow(u, 0.5e1)) * q4 + (0.48e2 * t - 0.96e2 * u) * pow(q3, 0.5e1) * pow(q4, 0.3e1) + (-0.144e3 * t * t - 0.32e2 * t * u) * pow(q3, 0.5e1) * q4 * q4 + (-0.32e2 * pow(t, 0.3e1) - 0.40e2 * t * t * u - 0.24e2 * t * u * u) * pow(q3, 0.5e1) * q4 + (-0.136e3 * t * t + 0.200e3 * t * u) * pow(q3, 0.4e1) * pow(q4, 0.3e1) + (0.220e3 * pow(t, 0.3e1) + 0.176e3 * t * t * u + 0.84e2 * t * u * u + 0.48e2 * pow(u, 0.3e1)) * pow(q3, 0.4e1) * q4 * q4 + (0.98e2 * pow(t, 0.4e1) + 0.138e3 * pow(t, 0.3e1) * u + 0.94e2 * t * t * u * u + 0.22e2 * t * pow(u, 0.3e1)) * pow(q3, 0.4e1) * q4 + (0.120e3 * pow(t, 0.3e1) - 0.48e2 * t * t * u - 0.24e2 * t * u * u) * pow(q3, 0.3e1) * pow(q4, 0.3e1) + (-0.176e3 * pow(t, 0.4e1) - 0.236e3 * pow(t, 0.3e1) * u - 0.240e3 * t * t * u * u - 0.100e3 * t * pow(u, 0.3e1)) * pow(q3, 0.3e1) * q4 * q4 + (-0.153e3 * pow(t, 0.5e1) - 0.308e3 * pow(t, 0.4e1) * u - 0.280e3 * pow(t, 0.3e1) * u * u - 0.134e3 * t * t * pow(u, 0.3e1) - 0.31e2 * t * pow(u, 0.4e1) - 0.6e1 * pow(u, 0.5e1)) * pow(q3, 0.3e1) * q4 + (0.8e1 * pow(t, 0.4e1) + 0.56e2 * pow(t, 0.3e1) * u + 0.64e2 * t * t * u * u) * q3 * q3 * pow(q4, 0.3e1) + (0.23e2 * pow(t, 0.5e1) - 0.4e1 * pow(t, 0.4e1) * u + 0.30e2 * pow(t, 0.3e1) * u * u + 0.12e2 * t * t * pow(u, 0.3e1) + 0.3e1 * t * pow(u, 0.4e1)) * q3 * q3 * q4 * q4 + (0.137e3 * pow(t, 0.6e1) + 0.359e3 * pow(t, 0.5e1) * u + 0.416e3 * pow(t, 0.4e1) * u * u + 0.284e3 * pow(t, 0.3e1) * pow(u, 0.3e1) + 0.103e3 * t * t * pow(u, 0.4e1) + 0.13e2 * t * pow(u, 0.5e1)) * q3 * q3 * q4 + (-0.32e2 * pow(t, 0.5e1) - 0.64e2 * pow(t, 0.4e1) * u - 0.32e2 * pow(t, 0.3e1) * u * u) * q3 * pow(q4, 0.3e1) + (0.24e2 * pow(t, 0.6e1) + 0.60e2 * pow(t, 0.5e1) * u + 0.48e2 * pow(t, 0.4e1) * u * u + 0.4e1 * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.8e1 * t * t * pow(u, 0.4e1)) * q3 * q4 * q4 + (-0.46e2 * pow(t, 0.7e1) - 0.127e3 * pow(t, 0.6e1) * u - 0.150e3 * pow(t, 0.5e1) * u * u - 0.104e3 * pow(t, 0.4e1) * pow(u, 0.3e1) - 0.36e2 * pow(t, 0.3e1) * pow(u, 0.4e1) - t * t * pow(u, 0.5e1)) * q3 * q4 + 0.90e2 * pow(t, 0.6e1) * pow(u, 0.3e1) + 0.65e2 * pow(t, 0.5e1) * pow(u, 0.4e1) + 0.27e2 * pow(t, 0.4e1) * pow(u, 0.5e1) + 0.5e1 * pow(t, 0.3e1) * pow(u, 0.6e1)) * pow(q3 - t, -0.2e1) / u * pow(t, -0.2e1) * pow(0.4e1 * q3 * q4 - t * t - 0.2e1 * t * u - u * u, -0.2e1));
}
#include <math.h>

double GstarVCoeffsC6e2 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return(0.16e2 * (0.8e1 * t * pow(q3, 0.4e1) * q4 + (0.4e1 * t - 0.20e2 * u) * pow(q3, 0.3e1) * q4 * q4 + (-0.12e2 * t * t - 0.12e2 * t * u) * pow(q3, 0.3e1) * q4 + (-0.4e1 * t * t * u - 0.4e1 * t * u * u) * pow(q3, 0.3e1) + 0.8e1 * t * q3 * q3 * pow(q4, 0.3e1) + (0.12e2 * t * t + 0.12e2 * t * u) * q3 * q3 * q4 * q4 + (0.25e2 * pow(t, 0.3e1) + 0.15e2 * t * t * u + 0.19e2 * t * u * u + 0.5e1 * pow(u, 0.3e1)) * q3 * q3 * q4 + (0.8e1 * pow(t, 0.3e1) * u + 0.12e2 * t * t * u * u + 0.4e1 * t * pow(u, 0.3e1)) * q3 * q3 + (0.2e1 * pow(t, 0.3e1) - 0.8e1 * t * t * u - 0.2e1 * t * u * u) * q3 * q4 * q4 + (-0.26e2 * pow(t, 0.4e1) - 0.30e2 * pow(t, 0.3e1) * u - 0.14e2 * t * t * u * u - 0.2e1 * t * pow(u, 0.3e1)) * q3 * q4 + (-0.5e1 * pow(t, 0.5e1) - 0.14e2 * pow(t, 0.4e1) * u - 0.16e2 * pow(t, 0.3e1) * u * u - 0.10e2 * t * t * pow(u, 0.3e1) - 0.3e1 * t * pow(u, 0.4e1)) * q3 + 0.5e1 * pow(t, 0.6e1) + 0.15e2 * pow(t, 0.5e1) * u + 0.17e2 * pow(t, 0.4e1) * u * u + 0.9e1 * pow(t, 0.3e1) * pow(u, 0.3e1) + 0.2e1 * t * t * pow(u, 0.4e1)) / (0.4e1 * q3 * q4 - t * t - 0.2e1 * t * u - u * u) * pow(t, -0.2e1) / u / (q3 - t));
}
double GstarVCoeffsC7em1 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return(-0.32e2 * q3 / t - 0.32e2 * q4 / t - 0.16e2 * t / u + 0.32e2 + 0.32e2 / t * u);
}
#include <math.h>

double GstarVCoeffsC7e0 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return(0.16e2 * ((0.16e2 * t + 0.16e2 * u) * pow(q3, 0.3e1) * q4 * q4 + (-0.6e1 * t * t - 0.4e1 * t * u + 0.18e2 * u * u) * pow(q3, 0.3e1) * q4 + (0.16e2 * t + 0.16e2 * u) * q3 * q3 * pow(q4, 0.3e1) + (0.36e2 * t * t - 0.40e2 * t * u - 0.12e2 * u * u) * q3 * q3 * q4 * q4 + (-0.4e1 * pow(t, 0.3e1) - 0.4e1 * t * t * u - 0.44e2 * t * u * u - 0.44e2 * pow(u, 0.3e1)) * q3 * q3 * q4 + (0.3e1 * pow(t, 0.4e1) + 0.4e1 * pow(t, 0.3e1) * u - 0.4e1 * t * t * u * u - 0.8e1 * t * pow(u, 0.3e1) - 0.3e1 * pow(u, 0.4e1)) * q3 * q3 + (-0.6e1 * t * t - 0.4e1 * t * u + 0.18e2 * u * u) * q3 * pow(q4, 0.3e1) + (-0.4e1 * pow(t, 0.3e1) - 0.4e1 * t * t * u - 0.44e2 * t * u * u - 0.44e2 * pow(u, 0.3e1)) * q3 * q4 * q4 + (-0.16e2 * pow(t, 0.4e1) - 0.32e2 * pow(t, 0.3e1) * u + 0.20e2 * t * t * u * u + 0.72e2 * t * pow(u, 0.3e1) + 0.36e2 * pow(u, 0.4e1)) * q3 * q4 + (-0.3e1 * pow(t, 0.5e1) - 0.5e1 * pow(t, 0.4e1) * u + 0.10e2 * pow(t, 0.3e1) * u * u + 0.30e2 * t * t * pow(u, 0.3e1) + 0.25e2 * t * pow(u, 0.4e1) + 0.7e1 * pow(u, 0.5e1)) * q3 + (0.3e1 * pow(t, 0.4e1) + 0.4e1 * pow(t, 0.3e1) * u - 0.4e1 * t * t * u * u - 0.8e1 * t * pow(u, 0.3e1) - 0.3e1 * pow(u, 0.4e1)) * q4 * q4 + (-0.3e1 * pow(t, 0.5e1) - 0.5e1 * pow(t, 0.4e1) * u + 0.10e2 * pow(t, 0.3e1) * u * u + 0.30e2 * t * t * pow(u, 0.3e1) + 0.25e2 * t * pow(u, 0.4e1) + 0.7e1 * pow(u, 0.5e1)) * q4 + 0.4e1 * pow(t, 0.6e1) + 0.14e2 * pow(t, 0.5e1) * u + 0.10e2 * pow(t, 0.4e1) * u * u - 0.20e2 * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.40e2 * t * t * pow(u, 0.4e1) - 0.26e2 * t * pow(u, 0.5e1) - 0.6e1 * pow(u, 0.6e1)) * pow(0.4e1 * q3 * q4 - t * t - 0.2e1 * t * u - u * u, -0.2e1) / t / u);
}
#include <math.h>

double GstarVCoeffsC7e1 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return(0.16e2 * ((0.16e2 * t + 0.16e2 * u) * pow(q3, 0.3e1) * q4 * q4 + (0.8e1 * t * u - 0.8e1 * u * u) * pow(q3, 0.3e1) * q4 + (0.16e2 * t + 0.16e2 * u) * q3 * q3 * pow(q4, 0.3e1) + (-0.96e2 * t * t + 0.80e2 * u * u) * q3 * q3 * q4 * q4 + (-0.40e2 * t * t * u - 0.32e2 * t * u * u + 0.8e1 * pow(u, 0.3e1)) * q3 * q3 * q4 + (-pow(t, 0.4e1) - 0.2e1 * pow(t, 0.3e1) * u + 0.2e1 * t * pow(u, 0.3e1) + pow(u, 0.4e1)) * q3 * q3 + (0.8e1 * t * u - 0.8e1 * u * u) * q3 * pow(q4, 0.3e1) + (-0.40e2 * t * t * u - 0.32e2 * t * u * u + 0.8e1 * pow(u, 0.3e1)) * q3 * q4 * q4 + (0.38e2 * pow(t, 0.4e1) + 0.100e3 * pow(t, 0.3e1) * u + 0.32e2 * t * t * u * u - 0.84e2 * t * pow(u, 0.3e1) - 0.54e2 * pow(u, 0.4e1)) * q3 * q4 + (pow(t, 0.5e1) + 0.7e1 * pow(t, 0.4e1) * u + 0.14e2 * pow(t, 0.3e1) * u * u + 0.10e2 * t * t * pow(u, 0.3e1) + t * pow(u, 0.4e1) - pow(u, 0.5e1)) * q3 + (-pow(t, 0.4e1) - 0.2e1 * pow(t, 0.3e1) * u + 0.2e1 * t * pow(u, 0.3e1) + pow(u, 0.4e1)) * q4 * q4 + (pow(t, 0.5e1) + 0.7e1 * pow(t, 0.4e1) * u + 0.14e2 * pow(t, 0.3e1) * u * u + 0.10e2 * t * t * pow(u, 0.3e1) + t * pow(u, 0.4e1) - pow(u, 0.5e1)) * q4 - 0.5e1 * pow(t, 0.6e1) - 0.23e2 * pow(t, 0.5e1) * u - 0.35e2 * pow(t, 0.4e1) * u * u - 0.10e2 * pow(t, 0.3e1) * pow(u, 0.3e1) + 0.25e2 * t * t * pow(u, 0.4e1) + 0.25e2 * t * pow(u, 0.5e1) + 0.7e1 * pow(u, 0.6e1)) * pow(0.4e1 * q3 * q4 - t * t - 0.2e1 * t * u - u * u, -0.2e1) / t / u);
}
#include <math.h>

double GstarVCoeffsC7e2 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return(0.16e2 * ((0.2e1 * t * t - 0.2e1 * u * u) * q3 * q3 + (0.12e2 * t * t - 0.4e1 * t * u - 0.24e2 * u * u) * q3 * q4 + (-0.2e1 * pow(t, 0.3e1) - 0.2e1 * t * t * u + 0.2e1 * t * u * u + 0.2e1 * pow(u, 0.3e1)) * q3 + (0.2e1 * t * t - 0.2e1 * u * u) * q4 * q4 + (-0.2e1 * pow(t, 0.3e1) - 0.2e1 * t * t * u + 0.2e1 * t * u * u + 0.2e1 * pow(u, 0.3e1)) * q4 - 0.2e1 * pow(t, 0.4e1) - 0.3e1 * pow(t, 0.3e1) * u + 0.5e1 * t * t * u * u + 0.11e2 * t * pow(u, 0.3e1) + 0.5e1 * pow(u, 0.4e1)) / u / t / (0.4e1 * q3 * q4 - t * t - 0.2e1 * t * u - u * u));
}
double GstarVCoeffsC8em1 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return(-0.32e2 * q3 / u - 0.32e2 * q4 / u + 0.32e2 * t / u + 0.32e2 - 0.16e2 / t * u);
}
#include <math.h>

double GstarVCoeffsC8e0 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return(0.16e2 * ((0.16e2 * t + 0.16e2 * u) * pow(q3, 0.3e1) * q4 * q4 + (0.18e2 * t * t - 0.4e1 * t * u - 0.6e1 * u * u) * pow(q3, 0.3e1) * q4 + (0.16e2 * t + 0.16e2 * u) * q3 * q3 * pow(q4, 0.3e1) + (-0.12e2 * t * t - 0.40e2 * t * u + 0.36e2 * u * u) * q3 * q3 * q4 * q4 + (-0.44e2 * pow(t, 0.3e1) - 0.44e2 * t * t * u - 0.4e1 * t * u * u - 0.4e1 * pow(u, 0.3e1)) * q3 * q3 * q4 + (-0.3e1 * pow(t, 0.4e1) - 0.8e1 * pow(t, 0.3e1) * u - 0.4e1 * t * t * u * u + 0.4e1 * t * pow(u, 0.3e1) + 0.3e1 * pow(u, 0.4e1)) * q3 * q3 + (0.18e2 * t * t - 0.4e1 * t * u - 0.6e1 * u * u) * q3 * pow(q4, 0.3e1) + (-0.44e2 * pow(t, 0.3e1) - 0.44e2 * t * t * u - 0.4e1 * t * u * u - 0.4e1 * pow(u, 0.3e1)) * q3 * q4 * q4 + (0.36e2 * pow(t, 0.4e1) + 0.72e2 * pow(t, 0.3e1) * u + 0.20e2 * t * t * u * u - 0.32e2 * t * pow(u, 0.3e1) - 0.16e2 * pow(u, 0.4e1)) * q3 * q4 + (0.7e1 * pow(t, 0.5e1) + 0.25e2 * pow(t, 0.4e1) * u + 0.30e2 * pow(t, 0.3e1) * u * u + 0.10e2 * t * t * pow(u, 0.3e1) - 0.5e1 * t * pow(u, 0.4e1) - 0.3e1 * pow(u, 0.5e1)) * q3 + (-0.3e1 * pow(t, 0.4e1) - 0.8e1 * pow(t, 0.3e1) * u - 0.4e1 * t * t * u * u + 0.4e1 * t * pow(u, 0.3e1) + 0.3e1 * pow(u, 0.4e1)) * q4 * q4 + (0.7e1 * pow(t, 0.5e1) + 0.25e2 * pow(t, 0.4e1) * u + 0.30e2 * pow(t, 0.3e1) * u * u + 0.10e2 * t * t * pow(u, 0.3e1) - 0.5e1 * t * pow(u, 0.4e1) - 0.3e1 * pow(u, 0.5e1)) * q4 - 0.6e1 * pow(t, 0.6e1) - 0.26e2 * pow(t, 0.5e1) * u - 0.40e2 * pow(t, 0.4e1) * u * u - 0.20e2 * pow(t, 0.3e1) * pow(u, 0.3e1) + 0.10e2 * t * t * pow(u, 0.4e1) + 0.14e2 * t * pow(u, 0.5e1) + 0.4e1 * pow(u, 0.6e1)) * pow(0.4e1 * q3 * q4 - t * t - 0.2e1 * t * u - u * u, -0.2e1) / t / u);
}
#include <math.h>

double GstarVCoeffsC8e1 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return(0.16e2 * ((0.16e2 * t + 0.16e2 * u) * pow(q3, 0.3e1) * q4 * q4 + (-0.8e1 * t * t + 0.8e1 * t * u) * pow(q3, 0.3e1) * q4 + (0.16e2 * t + 0.16e2 * u) * q3 * q3 * pow(q4, 0.3e1) + (0.80e2 * t * t - 0.96e2 * u * u) * q3 * q3 * q4 * q4 + (0.8e1 * pow(t, 0.3e1) - 0.32e2 * t * t * u - 0.40e2 * t * u * u) * q3 * q3 * q4 + (pow(t, 0.4e1) + 0.2e1 * pow(t, 0.3e1) * u - 0.2e1 * t * pow(u, 0.3e1) - pow(u, 0.4e1)) * q3 * q3 + (-0.8e1 * t * t + 0.8e1 * t * u) * q3 * pow(q4, 0.3e1) + (0.8e1 * pow(t, 0.3e1) - 0.32e2 * t * t * u - 0.40e2 * t * u * u) * q3 * q4 * q4 + (-0.54e2 * pow(t, 0.4e1) - 0.84e2 * pow(t, 0.3e1) * u + 0.32e2 * t * t * u * u + 0.100e3 * t * pow(u, 0.3e1) + 0.38e2 * pow(u, 0.4e1)) * q3 * q4 + (-pow(t, 0.5e1) + pow(t, 0.4e1) * u + 0.10e2 * pow(t, 0.3e1) * u * u + 0.14e2 * t * t * pow(u, 0.3e1) + 0.7e1 * t * pow(u, 0.4e1) + pow(u, 0.5e1)) * q3 + (pow(t, 0.4e1) + 0.2e1 * pow(t, 0.3e1) * u - 0.2e1 * t * pow(u, 0.3e1) - pow(u, 0.4e1)) * q4 * q4 + (-pow(t, 0.5e1) + pow(t, 0.4e1) * u + 0.10e2 * pow(t, 0.3e1) * u * u + 0.14e2 * t * t * pow(u, 0.3e1) + 0.7e1 * t * pow(u, 0.4e1) + pow(u, 0.5e1)) * q4 + 0.7e1 * pow(t, 0.6e1) + 0.25e2 * pow(t, 0.5e1) * u + 0.25e2 * pow(t, 0.4e1) * u * u - 0.10e2 * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.35e2 * t * t * pow(u, 0.4e1) - 0.23e2 * t * pow(u, 0.5e1) - 0.5e1 * pow(u, 0.6e1)) * pow(0.4e1 * q3 * q4 - t * t - 0.2e1 * t * u - u * u, -0.2e1) / t / u);
}
#include <math.h>

double GstarVCoeffsC8e2 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return(-0.16e2 * ((0.2e1 * t * t - 0.2e1 * u * u) * q3 * q3 + (0.24e2 * t * t + 0.4e1 * t * u - 0.12e2 * u * u) * q3 * q4 + (-0.2e1 * pow(t, 0.3e1) - 0.2e1 * t * t * u + 0.2e1 * t * u * u + 0.2e1 * pow(u, 0.3e1)) * q3 + (0.2e1 * t * t - 0.2e1 * u * u) * q4 * q4 + (-0.2e1 * pow(t, 0.3e1) - 0.2e1 * t * t * u + 0.2e1 * t * u * u + 0.2e1 * pow(u, 0.3e1)) * q4 - 0.5e1 * pow(t, 0.4e1) - 0.11e2 * pow(t, 0.3e1) * u - 0.5e1 * t * t * u * u + 0.3e1 * t * pow(u, 0.3e1) + 0.2e1 * pow(u, 0.4e1)) / u / t / (0.4e1 * q3 * q4 - t * t - 0.2e1 * t * u - u * u));
}
double GstarVCoeffsC9em1 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return(0);
}
#include <math.h>

double GstarVCoeffsC9e0 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return(-0.16e2 * ((0.12e2 * t + 0.60e2 * u) * pow(q3, 0.4e1) * q4 * q4 + (-0.8e1 * t + 0.120e3 * u) * pow(q3, 0.3e1) * pow(q4, 0.3e1) + (-0.8e1 * t * t - 0.96e2 * t * u - 0.120e3 * u * u) * pow(q3, 0.3e1) * q4 * q4 + (-0.8e1 * pow(t, 0.3e1) - 0.48e2 * t * t * u - 0.60e2 * t * u * u - 0.20e2 * pow(u, 0.3e1)) * pow(q3, 0.3e1) * q4 + (0.12e2 * t + 0.60e2 * u) * q3 * q3 * pow(q4, 0.4e1) + (-0.8e1 * t * t - 0.96e2 * t * u - 0.120e3 * u * u) * q3 * q3 * pow(q4, 0.3e1) + (0.12e2 * pow(t, 0.3e1) - 0.28e2 * t * t * u - 0.20e2 * t * u * u + 0.20e2 * pow(u, 0.3e1)) * q3 * q3 * q4 * q4 + (0.4e1 * pow(t, 0.4e1) + 0.68e2 * pow(t, 0.3e1) * u + 0.164e3 * t * t * u * u + 0.140e3 * t * pow(u, 0.3e1) + 0.40e2 * pow(u, 0.4e1)) * q3 * q3 * q4 + (0.2e1 * pow(t, 0.5e1) + 0.10e2 * pow(t, 0.4e1) * u + 0.20e2 * pow(t, 0.3e1) * u * u + 0.20e2 * t * t * pow(u, 0.3e1) + 0.10e2 * t * pow(u, 0.4e1) + 0.2e1 * pow(u, 0.5e1)) * q3 * q3 + (-0.8e1 * pow(t, 0.3e1) - 0.48e2 * t * t * u - 0.60e2 * t * u * u - 0.20e2 * pow(u, 0.3e1)) * q3 * pow(q4, 0.3e1) + (0.4e1 * pow(t, 0.4e1) + 0.68e2 * pow(t, 0.3e1) * u + 0.164e3 * t * t * u * u + 0.140e3 * t * pow(u, 0.3e1) + 0.40e2 * pow(u, 0.4e1)) * q3 * q4 * q4 + (-0.2e1 * pow(t, 0.5e1) - 0.20e2 * pow(t, 0.4e1) * u - 0.64e2 * pow(t, 0.3e1) * u * u - 0.92e2 * t * t * pow(u, 0.3e1) - 0.62e2 * t * pow(u, 0.4e1) - 0.16e2 * pow(u, 0.5e1)) * q3 * q4 + (-0.2e1 * pow(t, 0.6e1) - 0.14e2 * pow(t, 0.5e1) * u - 0.40e2 * pow(t, 0.4e1) * u * u - 0.60e2 * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.50e2 * t * t * pow(u, 0.4e1) - 0.22e2 * t * pow(u, 0.5e1) - 0.4e1 * pow(u, 0.6e1)) * q3 + (0.2e1 * pow(t, 0.5e1) + 0.10e2 * pow(t, 0.4e1) * u + 0.20e2 * pow(t, 0.3e1) * u * u + 0.20e2 * t * t * pow(u, 0.3e1) + 0.10e2 * t * pow(u, 0.4e1) + 0.2e1 * pow(u, 0.5e1)) * q4 * q4 + (-0.2e1 * pow(t, 0.6e1) - 0.14e2 * pow(t, 0.5e1) * u - 0.40e2 * pow(t, 0.4e1) * u * u - 0.60e2 * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.50e2 * t * t * pow(u, 0.4e1) - 0.22e2 * t * pow(u, 0.5e1) - 0.4e1 * pow(u, 0.6e1)) * q4 + pow(t, 0.7e1) + 0.7e1 * pow(t, 0.6e1) * u + 0.22e2 * pow(t, 0.5e1) * u * u + 0.40e2 * pow(t, 0.4e1) * pow(u, 0.3e1) + 0.45e2 * pow(t, 0.3e1) * pow(u, 0.4e1) + 0.31e2 * t * t * pow(u, 0.5e1) + 0.12e2 * t * pow(u, 0.6e1) + 0.2e1 * pow(u, 0.7e1)) * pow(0.4e1 * q3 * q4 - t * t - 0.2e1 * t * u - u * u, -0.2e1) / t / u);
}
#include <math.h>

double GstarVCoeffsC9e1 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return(0.32e2 * ((0.12e2 * t + 0.20e2 * u) * pow(q3, 0.4e1) * q4 * q4 + (-0.24e2 * t - 0.8e1 * u) * pow(q3, 0.3e1) * pow(q4, 0.3e1) + (-0.16e2 * t * t - 0.24e2 * t * u - 0.24e2 * u * u) * pow(q3, 0.3e1) * q4 * q4 + (-0.6e1 * pow(t, 0.3e1) - 0.24e2 * t * t * u - 0.26e2 * t * u * u - 0.8e1 * pow(u, 0.3e1)) * pow(q3, 0.3e1) * q4 + (0.12e2 * t + 0.20e2 * u) * q3 * q3 * pow(q4, 0.4e1) + (-0.16e2 * t * t - 0.24e2 * t * u - 0.24e2 * u * u) * q3 * q3 * pow(q4, 0.3e1) + (0.32e2 * pow(t, 0.3e1) + 0.52e2 * t * t * u + 0.48e2 * t * u * u + 0.28e2 * pow(u, 0.3e1)) * q3 * q3 * q4 * q4 + (0.6e1 * pow(t, 0.4e1) + 0.30e2 * pow(t, 0.3e1) * u + 0.50e2 * t * t * u * u + 0.34e2 * t * pow(u, 0.3e1) + 0.8e1 * pow(u, 0.4e1)) * q3 * q3 * q4 + (pow(t, 0.5e1) + 0.5e1 * pow(t, 0.4e1) * u + 0.10e2 * pow(t, 0.3e1) * u * u + 0.10e2 * t * t * pow(u, 0.3e1) + 0.5e1 * t * pow(u, 0.4e1) + pow(u, 0.5e1)) * q3 * q3 + (-0.6e1 * pow(t, 0.3e1) - 0.24e2 * t * t * u - 0.26e2 * t * u * u - 0.8e1 * pow(u, 0.3e1)) * q3 * pow(q4, 0.3e1) + (0.6e1 * pow(t, 0.4e1) + 0.30e2 * pow(t, 0.3e1) * u + 0.50e2 * t * t * u * u + 0.34e2 * t * pow(u, 0.3e1) + 0.8e1 * pow(u, 0.4e1)) * q3 * q4 * q4 + (-0.9e1 * pow(t, 0.5e1) - 0.37e2 * pow(t, 0.4e1) * u - 0.66e2 * pow(t, 0.3e1) * u * u - 0.66e2 * t * t * pow(u, 0.3e1) - 0.37e2 * t * pow(u, 0.4e1) - 0.9e1 * pow(u, 0.5e1)) * q3 * q4 + (-pow(t, 0.6e1) - 0.6e1 * pow(t, 0.5e1) * u - 0.15e2 * pow(t, 0.4e1) * u * u - 0.20e2 * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.15e2 * t * t * pow(u, 0.4e1) - 0.6e1 * t * pow(u, 0.5e1) - pow(u, 0.6e1)) * q3 + (pow(t, 0.5e1) + 0.5e1 * pow(t, 0.4e1) * u + 0.10e2 * pow(t, 0.3e1) * u * u + 0.10e2 * t * t * pow(u, 0.3e1) + 0.5e1 * t * pow(u, 0.4e1) + pow(u, 0.5e1)) * q4 * q4 + (-pow(t, 0.6e1) - 0.6e1 * pow(t, 0.5e1) * u - 0.15e2 * pow(t, 0.4e1) * u * u - 0.20e2 * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.15e2 * t * t * pow(u, 0.4e1) - 0.6e1 * t * pow(u, 0.5e1) - pow(u, 0.6e1)) * q4 + pow(t, 0.7e1) + 0.6e1 * pow(t, 0.6e1) * u + 0.16e2 * pow(t, 0.5e1) * u * u + 0.25e2 * pow(t, 0.4e1) * pow(u, 0.3e1) + 0.25e2 * pow(t, 0.3e1) * pow(u, 0.4e1) + 0.16e2 * t * t * pow(u, 0.5e1) + 0.6e1 * t * pow(u, 0.6e1) + pow(u, 0.7e1)) * pow(0.4e1 * q3 * q4 - t * t - 0.2e1 * t * u - u * u, -0.2e1) / t / u);
}
#include <math.h>

double GstarVCoeffsC9e2 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return(-0.16e2 * ((0.4e1 * t - 0.4e1 * u) * pow(q3, 0.3e1) * q4 + (-0.16e2 * t - 0.40e2 * u) * q3 * q3 * q4 * q4 + (-0.4e1 * t * t + 0.4e1 * u * u) * q3 * q3 * q4 + (0.4e1 * t - 0.4e1 * u) * q3 * pow(q4, 0.3e1) + (-0.4e1 * t * t + 0.4e1 * u * u) * q3 * q4 * q4 + (0.10e2 * pow(t, 0.3e1) + 0.36e2 * t * t * u + 0.46e2 * t * u * u + 0.20e2 * pow(u, 0.3e1)) * q3 * q4 - pow(t, 0.5e1) - 0.6e1 * pow(t, 0.4e1) * u - 0.15e2 * pow(t, 0.3e1) * u * u - 0.19e2 * t * t * pow(u, 0.3e1) - 0.12e2 * t * pow(u, 0.4e1) - 0.3e1 * pow(u, 0.5e1)) / u / t / (0.4e1 * q3 * q4 - t * t - 0.2e1 * t * u - u * u));
}
double GstarVCoeffsC10em1 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return(0);
}
#include <math.h>

double GstarVCoeffsC10e0 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return(-0.16e2 * ((0.60e2 * t + 0.12e2 * u) * pow(q3, 0.4e1) * q4 * q4 + (0.120e3 * t - 0.8e1 * u) * pow(q3, 0.3e1) * pow(q4, 0.3e1) + (-0.120e3 * t * t - 0.96e2 * t * u - 0.8e1 * u * u) * pow(q3, 0.3e1) * q4 * q4 + (-0.20e2 * pow(t, 0.3e1) - 0.60e2 * t * t * u - 0.48e2 * t * u * u - 0.8e1 * pow(u, 0.3e1)) * pow(q3, 0.3e1) * q4 + (0.60e2 * t + 0.12e2 * u) * q3 * q3 * pow(q4, 0.4e1) + (-0.120e3 * t * t - 0.96e2 * t * u - 0.8e1 * u * u) * q3 * q3 * pow(q4, 0.3e1) + (0.20e2 * pow(t, 0.3e1) - 0.20e2 * t * t * u - 0.28e2 * t * u * u + 0.12e2 * pow(u, 0.3e1)) * q3 * q3 * q4 * q4 + (0.40e2 * pow(t, 0.4e1) + 0.140e3 * pow(t, 0.3e1) * u + 0.164e3 * t * t * u * u + 0.68e2 * t * pow(u, 0.3e1) + 0.4e1 * pow(u, 0.4e1)) * q3 * q3 * q4 + (0.2e1 * pow(t, 0.5e1) + 0.10e2 * pow(t, 0.4e1) * u + 0.20e2 * pow(t, 0.3e1) * u * u + 0.20e2 * t * t * pow(u, 0.3e1) + 0.10e2 * t * pow(u, 0.4e1) + 0.2e1 * pow(u, 0.5e1)) * q3 * q3 + (-0.20e2 * pow(t, 0.3e1) - 0.60e2 * t * t * u - 0.48e2 * t * u * u - 0.8e1 * pow(u, 0.3e1)) * q3 * pow(q4, 0.3e1) + (0.40e2 * pow(t, 0.4e1) + 0.140e3 * pow(t, 0.3e1) * u + 0.164e3 * t * t * u * u + 0.68e2 * t * pow(u, 0.3e1) + 0.4e1 * pow(u, 0.4e1)) * q3 * q4 * q4 + (-0.16e2 * pow(t, 0.5e1) - 0.62e2 * pow(t, 0.4e1) * u - 0.92e2 * pow(t, 0.3e1) * u * u - 0.64e2 * t * t * pow(u, 0.3e1) - 0.20e2 * t * pow(u, 0.4e1) - 0.2e1 * pow(u, 0.5e1)) * q3 * q4 + (-0.4e1 * pow(t, 0.6e1) - 0.22e2 * pow(t, 0.5e1) * u - 0.50e2 * pow(t, 0.4e1) * u * u - 0.60e2 * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.40e2 * t * t * pow(u, 0.4e1) - 0.14e2 * t * pow(u, 0.5e1) - 0.2e1 * pow(u, 0.6e1)) * q3 + (0.2e1 * pow(t, 0.5e1) + 0.10e2 * pow(t, 0.4e1) * u + 0.20e2 * pow(t, 0.3e1) * u * u + 0.20e2 * t * t * pow(u, 0.3e1) + 0.10e2 * t * pow(u, 0.4e1) + 0.2e1 * pow(u, 0.5e1)) * q4 * q4 + (-0.4e1 * pow(t, 0.6e1) - 0.22e2 * pow(t, 0.5e1) * u - 0.50e2 * pow(t, 0.4e1) * u * u - 0.60e2 * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.40e2 * t * t * pow(u, 0.4e1) - 0.14e2 * t * pow(u, 0.5e1) - 0.2e1 * pow(u, 0.6e1)) * q4 + 0.2e1 * pow(t, 0.7e1) + 0.12e2 * pow(t, 0.6e1) * u + 0.31e2 * pow(t, 0.5e1) * u * u + 0.45e2 * pow(t, 0.4e1) * pow(u, 0.3e1) + 0.40e2 * pow(t, 0.3e1) * pow(u, 0.4e1) + 0.22e2 * t * t * pow(u, 0.5e1) + 0.7e1 * t * pow(u, 0.6e1) + pow(u, 0.7e1)) * pow(0.4e1 * q3 * q4 - t * t - 0.2e1 * t * u - u * u, -0.2e1) / t / u);
}
#include <math.h>

double GstarVCoeffsC10e1 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return(0.32e2 * ((0.20e2 * t + 0.12e2 * u) * pow(q3, 0.4e1) * q4 * q4 + (-0.8e1 * t - 0.24e2 * u) * pow(q3, 0.3e1) * pow(q4, 0.3e1) + (-0.24e2 * t * t - 0.24e2 * t * u - 0.16e2 * u * u) * pow(q3, 0.3e1) * q4 * q4 + (-0.8e1 * pow(t, 0.3e1) - 0.26e2 * t * t * u - 0.24e2 * t * u * u - 0.6e1 * pow(u, 0.3e1)) * pow(q3, 0.3e1) * q4 + (0.20e2 * t + 0.12e2 * u) * q3 * q3 * pow(q4, 0.4e1) + (-0.24e2 * t * t - 0.24e2 * t * u - 0.16e2 * u * u) * q3 * q3 * pow(q4, 0.3e1) + (0.28e2 * pow(t, 0.3e1) + 0.48e2 * t * t * u + 0.52e2 * t * u * u + 0.32e2 * pow(u, 0.3e1)) * q3 * q3 * q4 * q4 + (0.8e1 * pow(t, 0.4e1) + 0.34e2 * pow(t, 0.3e1) * u + 0.50e2 * t * t * u * u + 0.30e2 * t * pow(u, 0.3e1) + 0.6e1 * pow(u, 0.4e1)) * q3 * q3 * q4 + (pow(t, 0.5e1) + 0.5e1 * pow(t, 0.4e1) * u + 0.10e2 * pow(t, 0.3e1) * u * u + 0.10e2 * t * t * pow(u, 0.3e1) + 0.5e1 * t * pow(u, 0.4e1) + pow(u, 0.5e1)) * q3 * q3 + (-0.8e1 * pow(t, 0.3e1) - 0.26e2 * t * t * u - 0.24e2 * t * u * u - 0.6e1 * pow(u, 0.3e1)) * q3 * pow(q4, 0.3e1) + (0.8e1 * pow(t, 0.4e1) + 0.34e2 * pow(t, 0.3e1) * u + 0.50e2 * t * t * u * u + 0.30e2 * t * pow(u, 0.3e1) + 0.6e1 * pow(u, 0.4e1)) * q3 * q4 * q4 + (-0.9e1 * pow(t, 0.5e1) - 0.37e2 * pow(t, 0.4e1) * u - 0.66e2 * pow(t, 0.3e1) * u * u - 0.66e2 * t * t * pow(u, 0.3e1) - 0.37e2 * t * pow(u, 0.4e1) - 0.9e1 * pow(u, 0.5e1)) * q3 * q4 + (-pow(t, 0.6e1) - 0.6e1 * pow(t, 0.5e1) * u - 0.15e2 * pow(t, 0.4e1) * u * u - 0.20e2 * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.15e2 * t * t * pow(u, 0.4e1) - 0.6e1 * t * pow(u, 0.5e1) - pow(u, 0.6e1)) * q3 + (pow(t, 0.5e1) + 0.5e1 * pow(t, 0.4e1) * u + 0.10e2 * pow(t, 0.3e1) * u * u + 0.10e2 * t * t * pow(u, 0.3e1) + 0.5e1 * t * pow(u, 0.4e1) + pow(u, 0.5e1)) * q4 * q4 + (-pow(t, 0.6e1) - 0.6e1 * pow(t, 0.5e1) * u - 0.15e2 * pow(t, 0.4e1) * u * u - 0.20e2 * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.15e2 * t * t * pow(u, 0.4e1) - 0.6e1 * t * pow(u, 0.5e1) - pow(u, 0.6e1)) * q4 + pow(t, 0.7e1) + 0.6e1 * pow(t, 0.6e1) * u + 0.16e2 * pow(t, 0.5e1) * u * u + 0.25e2 * pow(t, 0.4e1) * pow(u, 0.3e1) + 0.25e2 * pow(t, 0.3e1) * pow(u, 0.4e1) + 0.16e2 * t * t * pow(u, 0.5e1) + 0.6e1 * t * pow(u, 0.6e1) + pow(u, 0.7e1)) * pow(0.4e1 * q3 * q4 - t * t - 0.2e1 * t * u - u * u, -0.2e1) / t / u);
}
#include <math.h>

double GstarVCoeffsC10e2 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return(0.16e2 * ((0.4e1 * t - 0.4e1 * u) * pow(q3, 0.3e1) * q4 + (0.40e2 * t + 0.16e2 * u) * q3 * q3 * q4 * q4 + (-0.4e1 * t * t + 0.4e1 * u * u) * q3 * q3 * q4 + (0.4e1 * t - 0.4e1 * u) * q3 * pow(q4, 0.3e1) + (-0.4e1 * t * t + 0.4e1 * u * u) * q3 * q4 * q4 + (-0.20e2 * pow(t, 0.3e1) - 0.46e2 * t * t * u - 0.36e2 * t * u * u - 0.10e2 * pow(u, 0.3e1)) * q3 * q4 + 0.3e1 * pow(t, 0.5e1) + 0.12e2 * pow(t, 0.4e1) * u + 0.19e2 * pow(t, 0.3e1) * u * u + 0.15e2 * t * t * pow(u, 0.3e1) + 0.6e1 * t * pow(u, 0.4e1) + pow(u, 0.5e1)) / u / t / (0.4e1 * q3 * q4 - t * t - 0.2e1 * t * u - u * u));
}
double GstarVCoeffsC11em1 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return(0);
}
#include <math.h>

double GstarVCoeffsC11e0 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return(-0.32e2 / t * pow(q3, 0.3e1) + (0.32e2 / u - 0.96e2 / t) * q3 * q3 * q4 + (0.64e2 + 0.96e2 / t * u) * q3 * q3 + (0.32e2 / u - 0.96e2 / t) * q3 * q4 * q4 + (-0.32e2 * t / u + 0.96e2 + 0.192e3 / t * u) * q3 * q4 + (-0.48e2 * t - 0.128e3 * u - 0.96e2 / t * u * u) * q3 - 0.32e2 / t * pow(q4, 0.3e1) + (0.64e2 + 0.96e2 / t * u) * q4 * q4 + (-0.48e2 * t - 0.128e3 * u - 0.96e2 / t * u * u) * q4 + 0.16e2 * t * t + 0.48e2 * t * u + 0.64e2 * u * u + 0.32e2 / t * pow(u, 0.3e1));
}
#include <math.h>

double GstarVCoeffsC11e1 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return(0.32e2 / t * pow(q3, 0.3e1) + (-0.64e2 / u + 0.64e2 / t) * q3 * q3 * q4 + (-0.64e2 - 0.64e2 / t * u) * q3 * q3 + (-0.64e2 / u + 0.64e2 / t) * q3 * q4 * q4 + (0.64e2 * t / u - 0.32e2 - 0.96e2 / t * u) * q3 * q4 + (0.64e2 * t + 0.96e2 * u + 0.64e2 / t * u * u) * q3 + 0.32e2 / t * pow(q4, 0.3e1) + (-0.64e2 - 0.64e2 / t * u) * q4 * q4 + (0.64e2 * t + 0.96e2 * u + 0.64e2 / t * u * u) * q4 - 0.32e2 * t * t - 0.64e2 * t * u - 0.64e2 * u * u - 0.32e2 / t * pow(u, 0.3e1));
}
#include <math.h>

double GstarVCoeffsC11e2 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return((0.32e2 / u + 0.32e2 / t) * q3 * q3 * q4 + (0.32e2 / u + 0.32e2 / t) * q3 * q4 * q4 + (-0.32e2 * t / u - 0.64e2 - 0.32e2 / t * u) * q3 * q4 + (-0.16e2 * t - 0.48e2 * u - 0.48e2 / t * u * u) * q3 + (-0.16e2 * t - 0.48e2 * u - 0.48e2 / t * u * u) * q4 + 0.16e2 * t * t + 0.64e2 * t * u + 0.96e2 * u * u + 0.48e2 / t * pow(u, 0.3e1));
}

double GstarVCoeffsC11e3 (
                          const vector<double>& kk)
{
    const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
    return -(16.*(q3+q4-t-u))*u*(t+u)/t ;
}





double GstarVCoeffsC12em1 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return(0);
}
#include <math.h>

double GstarVCoeffsC12e0 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return(-0.32e2 / u * pow(q3, 0.3e1) + (-0.96e2 / u + 0.32e2 / t) * q3 * q3 * q4 + (0.96e2 * t / u + 0.64e2) * q3 * q3 + (-0.96e2 / u + 0.32e2 / t) * q3 * q4 * q4 + (0.192e3 * t / u + 0.96e2 - 0.32e2 / t * u) * q3 * q4 + (-0.96e2 * t * t / u - 0.128e3 * t - 0.48e2 * u) * q3 - 0.32e2 / u * pow(q4, 0.3e1) + (0.96e2 * t / u + 0.64e2) * q4 * q4 + (-0.96e2 * t * t / u - 0.128e3 * t - 0.48e2 * u) * q4 + 0.32e2 * pow(t, 0.3e1) / u + 0.64e2 * t * t + 0.48e2 * t * u + 0.16e2 * u * u);
}
#include <math.h>

double GstarVCoeffsC12e1 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return(0.32e2 / u * pow(q3, 0.3e1) + (0.64e2 / u - 0.64e2 / t) * q3 * q3 * q4 + (-0.64e2 * t / u - 0.64e2) * q3 * q3 + (0.64e2 / u - 0.64e2 / t) * q3 * q4 * q4 + (-0.96e2 * t / u - 0.32e2 + 0.64e2 / t * u) * q3 * q4 + (0.64e2 * t * t / u + 0.96e2 * t + 0.64e2 * u) * q3 + 0.32e2 / u * pow(q4, 0.3e1) + (-0.64e2 * t / u - 0.64e2) * q4 * q4 + (0.64e2 * t * t / u + 0.96e2 * t + 0.64e2 * u) * q4 - 0.32e2 * pow(t, 0.3e1) / u - 0.64e2 * t * t - 0.64e2 * t * u - 0.32e2 * u * u);
}
#include <math.h>

double GstarVCoeffsC12e2 (
  const vector<double>& kk)
{
  const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
  return((0.32e2 / u + 0.32e2 / t) * q3 * q3 * q4 + (0.32e2 / u + 0.32e2 / t) * q3 * q4 * q4 + (-0.32e2 * t / u - 0.64e2 - 0.32e2 / t * u) * q3 * q4 + (-0.48e2 * t * t / u - 0.48e2 * t - 0.16e2 * u) * q3 + (-0.48e2 * t * t / u - 0.48e2 * t - 0.16e2 * u) * q4 + 0.48e2 * pow(t, 0.3e1) / u + 0.96e2 * t * t + 0.64e2 * t * u + 0.16e2 * u * u);
}

double GstarVCoeffsC12e3 (
                          const vector<double>& kk)
{
    const double q3 = kk[0];const double q4 = kk[1];const double u = kk[2];const double t = kk[3];
    return -(16.*(q3+q4-t-u))*t*(t+u)/u;
}








