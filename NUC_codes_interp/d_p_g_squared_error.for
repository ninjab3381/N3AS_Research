PROGRAM squared_err_val 

real T9_arr

OPEN(unit = 95, FILE="squared_err_arr_values_Nature_with_rate.dat", status = 'old')


DO i = 1, 60
    read(87, *) T9_arr(i),  sigma_v_d_p_g_3He_arr(i)

END DO

end program squared_err_val

    
