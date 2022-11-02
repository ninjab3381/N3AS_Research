program sigma_v_calculation_1
! This program calculates sigma_v depending on given sigma or s_factor     
    implicit none
   
    ! Type declarations
      !  double precision :: E_G, A_1, A_0, A, sigma_v_int_constant, sigma_v_int, & 
      !  sigma_v, new_sigma_v, E_root, E_root_keV, S_root, f

      !  double precision, dimension(13):: S, E
       
      !  double precision, dimension(60) :: T9
       
      !  double precision, dimension(6) :: GL_x_root, GL_w_root

      !  double precision :: PI

      real :: E_G, A_1, A_0, A, sigma_v_int_constant, sigma_v_int, & 
      sigma_v, new_sigma_v, E_root, E_root_keV, S_root, f, dS_dE

       real, dimension(13):: S, E
       
       real, dimension(60) :: T9
       
       real, dimension(6) :: GL_x_root, GL_w_root

       real :: PI



       INTEGER :: Z_0,Z_1
       integer :: i, j
       
    ! Executable statements 
       !Get variables for deuterium burning reaction d(p, g)3He
       PI = ACOS(-1.0)
       Z_0 = 1 !deuterium atomic umber
       Z_1 = 1 !proton atomic number
       S = (/ 0.386, 0.627, 0.850, 0.966, 1.133, 1.223, 1.375, 1.475, 1.648, 1.791, 1.866, 2.073, 2.156 /)
       S = S*10**(-6.0)
       E =  (/ 32.4, 66.7, 99.5, 115.9, 132.9, 149.3, 166.1, 182.7, 199.5, 222.8, 232.9, 252.9, 262.9 /)
       GL_x_root = (/ 0.22284660417926068946, 1.1889321016726230307, 2.9927363260593140777, & 
       5.7751435691045105018, 9.8374674183825899177, 15.982873980601701783 /)
       GL_w_root = (/ 0.45896467394996359357, 0.41700083077212099411, 0.11337338207404497574, & 
       0.010399197453149074899, 0.00026101720281493205948, 8.9854790642962123883e-7 /)
       
       E = E*10**(-3.0)
       A_1 = 1.007277 !atomic mass of proton
       A_0 = 2.01410177811 ! atomic mass of deuterium
       A = (A_0*A_1)/(A_0+A_1) !reduced atomic mass

       E_G = (0.98948*Z_0*Z_1*SQRT(A))**2

       ! T9 = (/ 40.0, 20.0, 10.0, 5.0, 2.5, 1.25, 1.0, 0.8, 0.6, 0.4, 0.2, 0.1, 0.005, 0.0025 /)

       T9 = (/0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008, 0.009,0.010, 0.011, 0.012,0.013, 0.014, & 
       0.015, 0.016, 0.018, 0.020, 0.025, 0.030, 0.040, 0.050, 0.060, 0.070, 0.080, 0.090, 0.100, 0.110, 0.120, 0.130, &
       0.140, 0.150, 0.160, 0.180, 0.200, 0.250, 0.300, 0.350, 0.400, 0.450, 0.500, 0.600, 0.700, 0.800, 0.900, 1.000, &
       1.250, 1.500, 1.750, 2.000, 2.500, 3.000, 3.500, 4.000, 5.000, 6.000, 7.000, 8.000, 9.000, 10.00 /)

      
      
      E_root_keV = 2000.0

      S_root = ((0.2121 + 5.973*10.0**(-3.0)*E_root_keV) + (5.449*10.0**(-6.0)*E_root_keV**(2.0)) -  & 
      (1.656*10.0**(-9.0)*E_root_keV**(3.0))) * 10.0**(-6.0)

      dS_dE = ((5.973*10.0**(-3.0)) + (5.449*10.0**(-6.0)*2.0*E_root_keV) -  & 
      (1.656*10.0**(-9.0)*3.0*E_root_keV**(2.0))) * 10.0**(-6.0)

      write(*,*) E_root_keV, S_root, dS_dE

 
    
end program sigma_v_calculation_1