/// \file
/// \ingroup tutorial_fit
/// \notebook -js
/// Example for fitting signal/background.
/// This example can be executed with:
///
/// ~~~{.cpp}
/// root > .x FittingDemo.C  (using the CINT interpreter)
/// root > .x FittingDemo.C+ (using the native complier via ACLIC)
/// ~~~
///
/// \macro_image
/// \macro_output
/// \macro_code
///
/// \author Rene Brun
 
#include "TH1.h"
#include "TMath.h"
#include "TF1.h"
#include "TLegend.h"
#include "TCanvas.h"
#include "Fit/Fitter.h"
#include <Math/Functor.h>
#include <iostream>
#include <fstream>
#include <cstdlib> 

const int n1_size = 13;
const int n2_size = 19;


double S_1[n1_size] = {0.386, 0.627, 0.850, 0.966, 1.133, 1.223, 1.375, 1.475, 1.648, 1.791, 1.866, 2.073, 2.156};

double E_1[n1_size] = {32.4, 66.7, 99.5, 115.9, 132.9, 149.3, 166.1, 182.7, 199.5, 222.8, 232.9, 252.9, 262.9};

double sys_std_1[n1_size] = {0.010, 0.016, 0.021, 0.024, 0.031, 0.031, 0.036,
                              0.037, 0.043, 0.045, 0.051, 0.052, 0.054};

double stat_std_1[n1_size] = {0.014, 0.009,0.008, 0.009, 0.004, 0.006, 0.004,
                              0.006, 0.003,0.006, 0.012, 0.012, 0.020 };   





double E_2[n2_size] = {10.0, 20.0, 35.0, 50.0, 70.0, 95.0, 120.0, 145.0, 170.0, 195.0,
                220.0, 245.0, 260.0, 300.0, 400.0, 500.0, 750.0, 1000.0, 2000.0};

double S_2[n2_size] = {0.286, 0.355, 0.460, 0.570, 0.716, 0.912, 1.112,
                1.317, 1.529, 1.748, 1.968, 2.197, 2.343, 2.716, 
                3.676, 4.739, 7.539, 10.685, 25.908};

double stat_std_2_percent[n2_size] = {0.1, 1.0, 1.1, 0.9, 0.4, 0.3, 0.8, 0.4, 0.4, 
0.4,0.5, 0.5, 0.9, 0.5, 0.6, 0.2, 0.3, 0.4, 0.0};
 

double sys_std_2_percent[n2_size] = {0.8, 1.1, 1.3, 1.7, 2.1, 2.3, 2.4, 2.5, 2.6, 2.6, 2.8, 
                              2.7, 2.8, 2.7, 2.7, 2.7, 2.6, 2.7, 2.3};
                    






double sFactor(double *x, const double *par) {
   return par[0] + par[1]*x[0] + par[2]*x[0]*x[0] + par[3]*x[0]*x[0]*x[0];
}


 
void dp_gamma_sigma_v_fit() {
 //Bevington Exercise by Peter Malzacher, modified by Rene Brun

 // Quadratic background function

   double stat_std_2[n2_size];

   for (int i = 0; i < n2_size; i++) {
      stat_std_2[i] = stat_std_2_percent[i] * S_2[i];
   }

   double sys_std_2[n2_size];
   for (int i = 0; i < n2_size; i++) {
      sys_std_2[i] = sys_std_2_percent[i] * S_2[i];
   }  

   double sys_error_min_1 = sys_std_1[0]/E_1[0];

   for (int i = 0; i<n1_size; i++) {
      if (sys_std_1[i]/E_1[i] < sys_error_min_1) {
         sys_error_min_1 = sys_std_1[i]/E_1[i];
      }
   }

   double sys_error_min_2 = sys_std_2[0]/E_2[0];

   for (int i = 0; i<n2_size; i++) {
      if (sys_std_2[i]/E_2[i] < sys_error_min_2) {
         sys_error_min_2 = sys_std_2[i]/E_2[i];
      }
   }

   auto chi2Function = [&](const double*par)  {

      double chi2_stat = 0;
      double chi2_norm = 0;
      double chi2_val = 0;
      for (int i=0;i<n1_size;i++) {

         chi2_stat = std::pow((sFactor(&E_1[i], par) - S_1[i]*par[4])/(par[4]*stat_std_1[i]), 2); 

         chi2_norm = std::pow((par[4]-1)/sys_error_min_1, 2);

         chi2_val += (chi2_stat + chi2_norm);
      }

      for (int i = 0; i<n2_size; i++) {
         if(stat_std_2[i] > 0) {
            chi2_stat = std::pow((sFactor(&E_2[i], par) - S_2[i]*par[5])/(par[5]*stat_std_2[i]), 2); 

            chi2_norm = std::pow((par[5]-1)/sys_error_min_2, 2);

            chi2_val += (chi2_stat + chi2_norm);
         }

      }


      return chi2_val;
   };

   ROOT::Math::Functor fcn(chi2Function,6);
   ROOT::Fit::Fitter  fitter;

   double pStart[6] = {0.2121, 5.973*(std::pow(10, -3)), 5.449*(std::pow(10, -6)), -1.656*(std::pow(10, -9)), 1, 1};
      
   fitter.SetFCN(fcn, pStart);

   fitter.Config().ParSettings(0).SetName("a1");
   fitter.Config().ParSettings(1).SetName("a2");
   fitter.Config().ParSettings(2).SetName("a3");
   fitter.Config().ParSettings(3).SetName("a4");
   fitter.Config().ParSettings(4).SetName("w_1");
   fitter.Config().ParSettings(5).SetName("w_2");


   //ROOT::Math::MinimizerOptions::SetDefaultMinimizer("Minuit2", "Migrad");


   bool ok = fitter.FitFCN();
   if (!ok) {
      Error("line3Dfit","Line3D Fit failed");
   }
 
   const ROOT::Fit::FitResult & result = fitter.Result();
   result.Print(std::cout, true);

   double cov_matrix_el = result.CovMatrix(1,1);

   ofstream outdata;

   outdata.open("cov_matrix_values.dat");
   for(int i = 0; i<4; i++) {
      for(int j = 0; j<4; j++) {
         cov_matrix_el = result.CovMatrix(i, j);

         if(i==3 && j==3) {
            outdata << cov_matrix_el;
         }
         else if(j==3) {
            outdata << cov_matrix_el << std::endl;
         }
         else {
            outdata << cov_matrix_el << " ";
         }
      }
   }

   outdata.close();


   const double *fitpar = result.GetParams();

   outdata.open("S_theory_values.dat");
   for(int i = 0; i<3; i++) {
      outdata << fitpar[i] << std::endl;
   }
   outdata << fitpar[3];

   outdata.close();
   

   // double a1 = fitpar[0];
   // double a2 = fitpar[1];
   // double a3 = fitpar[2];
   // double a4 = fitpar[3];
   // double w_k = fitpar[4];

   // std::cout << "a1 = " << a1 << ", a2 = " << a2 << ", a3 = " << a3 << ", a4 = " << a4 <<", w_k = " << w_k << std::endl;


 
}

