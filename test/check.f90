program check

    use, intrinsic :: iso_fortran_env , only: real64

    use, non_intrinsic :: integrate_inv_r_on_rectangle_fortran , only: integrate => integrate_inv_r_on_rectangle



    implicit none



    print *, ' 0.23806163576061183820921281760012077316'
    print *, integrate( inf_x =  0.5_real64 , sup_x =  1.0_real64 , inf_y =  0.5_real64 , sup_y =  1.0_real64 )
    print *, integrate( inf_x = -1.0_real64 , sup_x = -0.5_real64 , inf_y =  0.5_real64 , sup_y =  1.0_real64 )
    print *, integrate( inf_x = -1.0_real64 , sup_x = -0.5_real64 , inf_y = -1.0_real64 , sup_y = -0.5_real64 )
    print *, integrate( inf_x =  0.5_real64 , sup_x =  1.0_real64 , inf_y = -1.0_real64 , sup_y = -0.5_real64 )

    print *

    print *, '  7.0509886961563442018608745998383384722'
    print *, integrate( inf_x = -1.0_real64 , sup_x = 1.0_real64 , inf_y = -1.0_real64 , sup_y = 1.0_real64 )

    print *

    print *, '  3.5254943480781721009304372999191692361'
    print *, integrate( inf_x =  0.0_real64 , sup_x = 1.0_real64 , inf_y = -1.0_real64 , sup_y = 1.0_real64 )
    print *, integrate( inf_x = -1.0_real64 , sup_x = 1.0_real64 , inf_y =  0.0_real64 , sup_y = 1.0_real64 )
    print *, integrate( inf_x = -1.0_real64 , sup_x = 0.0_real64 , inf_y = -1.0_real64 , sup_y = 1.0_real64 )
    print *, integrate( inf_x = -1.0_real64 , sup_x = 1.0_real64 , inf_y = -1.0_real64 , sup_y = 0.0_real64 )

    print *

    print *, '  1.7627471740390860504652186499595846181'
    print *, integrate( inf_x =  0.0_real64 , sup_x = 1.0_real64 , inf_y =  0.0_real64 , sup_y = 1.0_real64 )
    print *, integrate( inf_x = -1.0_real64 , sup_x = 0.0_real64 , inf_y =  0.0_real64 , sup_y = 1.0_real64 )
    print *, integrate( inf_x = -1.0_real64 , sup_x = 0.0_real64 , inf_y = -1.0_real64 , sup_y = 0.0_real64 )
    print *, integrate( inf_x =  0.0_real64 , sup_x = 1.0_real64 , inf_y = -1.0_real64 , sup_y = 0.0_real64 )

end program check
