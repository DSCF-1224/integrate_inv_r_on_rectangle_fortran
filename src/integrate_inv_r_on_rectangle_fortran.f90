module integrate_inv_r_on_rectangle_fortran

    use, intrinsic :: iso_fortran_env , only: real64

    use, intrinsic :: ieee_arithmetic , only: ieee_signaling_nan
    use, intrinsic :: ieee_arithmetic , only: ieee_value



    implicit none



    private
    public  :: integrate_inv_r_on_rectangle



    contains



    elemental function integrate_inv_r_on_rectangle(inf_x, sup_x, inf_y, sup_y) result(integral)

        real(real64), intent(in) :: inf_x !! lower x boundary
        real(real64), intent(in) :: sup_x !! upper x boundary
        real(real64), intent(in) :: inf_y !! lower y boundary
        real(real64), intent(in) :: sup_y !! upper y boundary



        real(real64) :: integral !! integral value or NaN for invalid rectangle



        if ( (sup_x .lt. inf_x) .or. (sup_y .lt. inf_y) ) then

            integral = ieee_value(integral, ieee_signaling_nan)

            return

        end if



        if ( (inf_x .lt. sup_x) .and. (inf_y .lt. sup_y) ) then ! [inf_x,sup_x] x [inf_y,sup_y]

            if (inf_x .lt. 0.0_real64) then ! [n,?] x [?,?]

                if (sup_x .lt. 0.0_real64) then ! [n,n] x [?,?]

                    if (inf_y .lt. 0.0_real64) then ! [n,n] x [n,?]

                        if ( abs(sup_y) .gt. 0.0_real64 ) then ! [n,n] x [n,n] or [n,n] x [n,p]

                            integral = integrate_inv_r_on_rectangle_4(inf_x = inf_x, sup_x = sup_x, inf_y = inf_y, sup_y = sup_y)

                        else ! [n,n] x [n,z]

                            integral = integrate_inv_r_on_rectangle_3(sup_x = - inf_y, inf_y = inf_x, sup_y = sup_x)

                        end if

                    else if (inf_y .gt. 0.0_real64) then ! [n,n] x [p,p]

                        integral = integrate_inv_r_on_rectangle_4(inf_x = inf_x, sup_x = sup_x, inf_y = inf_y, sup_y = sup_y)

                    else ! [n,n] x [z,p]

                        integral = integrate_inv_r_on_rectangle_3(sup_x = inf_y, inf_y = inf_x, sup_y = sup_x)

                    end if

                else if (sup_x .gt. 0.0_real64) then ! [n,p] x [?,?]

                    if (inf_y .lt. 0.0_real64) then ! [n,p] x [n,?]

                        if ( abs(sup_y) .gt. 0.0_real64 ) then ! [n,p] x [n,n] or [n,p] x [n,p]

                            integral = integrate_inv_r_on_rectangle_4(inf_x = inf_x, sup_x = sup_x, inf_y = inf_y, sup_y = sup_y)

                        else ! [n,p] x [n,z]

                            integral = integrate_inv_r_on_rectangle_3(sup_x = - inf_y, inf_y = inf_x, sup_y = sup_x)

                        end if

                    else if (inf_y .gt. 0.0_real64) then ! [n,p] x [p,p]

                        integral = integrate_inv_r_on_rectangle_4(inf_x = inf_x, sup_x = sup_x, inf_y = inf_y, sup_y = sup_y)

                    else ! [n,p] x [z,p]

                        integral = integrate_inv_r_on_rectangle_3(sup_x = sup_y, inf_y = inf_x, sup_y = sup_x)

                    end if

                else ! [n,z] x [?,?]

                    if (inf_y .lt. 0.0_real64) then ! [n,z] x [n,?]

                        if ( abs(sup_y) .gt. 0.0_real64 ) then ! [n,z] x [n,n] or [n,z] x [n,p]

                            integral = integrate_inv_r_on_rectangle_3(sup_x = - inf_x, inf_y = inf_y, sup_y = sup_y)

                        else ! [n,z] x [n,z]

                            integral = integrate_inv_r_on_rectangle_2(sup_x = - inf_x, sup_y = - inf_y)

                        end if

                    else if (inf_y .gt. 0.0_real64) then ! [n,z] x [p,p]

                        integral = integrate_inv_r_on_rectangle_3(sup_x = - inf_x, inf_y = inf_y, sup_y = sup_y)

                    else ! [n,z] x [z,p]

                        integral = integrate_inv_r_on_rectangle_2(sup_x = - inf_x, sup_y = sup_y)

                    end if

                end if

            else if (inf_x .gt. 0.0_real64) then ! [p,p] x [?,?]

                if (inf_y .lt. 0.0_real64) then ! [p,p] x [n,?]

                    if ( abs(sup_y) .gt. 0.0_real64 ) then ! [p,p] x [n,n] or [p,p] x [n,p]

                        integral = integrate_inv_r_on_rectangle_4(inf_x = inf_x, sup_x = sup_x, inf_y = inf_y, sup_y = sup_y)

                    else ! [p,p] x [n,z]

                        integral = integrate_inv_r_on_rectangle_3(sup_x = - inf_y, inf_y = inf_x, sup_y = sup_x)

                    end if

                else if (inf_y .gt. 0.0_real64) then ! [p,p] x [p,p]

                    integral = integrate_inv_r_on_rectangle_4(inf_x = inf_x, sup_x = sup_x, inf_y = inf_y, sup_y = sup_y)

                else ! [p,p] x [z,p]

                    integral = integrate_inv_r_on_rectangle_3(sup_x = sup_y, inf_y = inf_x, sup_y = sup_x)

                end if

            else ! [z,p] x [?,?]

                if (inf_y .lt. 0.0_real64) then ! [z,p] x [n,?]

                    if ( abs(sup_y) .gt. 0.0_real64 ) then ! [z,p] x [n,n] or [z,p] x [n,p]

                        integral = integrate_inv_r_on_rectangle_3(sup_x = sup_x, inf_y = inf_y, sup_y = sup_y)

                    else ! [z,p] x [n,z]

                        integral = integrate_inv_r_on_rectangle_2(sup_x = sup_x, sup_y = - inf_y)

                    end if

                else if (inf_y .gt. 0.0_real64) then ! [z,p] x [p,p]

                    integral = integrate_inv_r_on_rectangle_3(sup_x = sup_x, inf_y = inf_y, sup_y = sup_y)

                else ! [z,p] x [z,p]

                    integral = integrate_inv_r_on_rectangle_2(sup_x = sup_x, sup_y = sup_y)

                end if

            end if

            return

        end if



        integral = 0.0_real64

    end function integrate_inv_r_on_rectangle



    !> @note
    !> J[a_,b_,c_,d_]:=Integrate[1/Sqrt[x^2+y^2], {x,y} \[Element] Rectangle[{a,c},{b,d}]]
    !> J[0,b,0,d] // InputForm
    !> @endnote
    elemental function integrate_inv_r_on_rectangle_2(sup_x, sup_y) result(integral)

        real(real64), intent(in) :: sup_x !! upper x boundary
        real(real64), intent(in) :: sup_y !! upper y boundary



        real(real64) :: integral



        associate(b => sup_x, d => sup_y)

            associate( r_bd => hypot(b,d) )

                associate( r_bd_b => (r_bd + b) )

                    integral = d * log( (r_bd_b * r_bd_b) / (d * d) ) &!
                    &        + b * log( 1.0_real64 + ( 2.0_real64 * d * (d + r_bd) ) / (b * b) )

                    integral = integral * 0.5_real64

                end associate

            end associate

        end associate

    end function integrate_inv_r_on_rectangle_2



    !> @note
    !> J[a_,b_,c_,d_]:=Integrate[1/Sqrt[x^2+y^2], {x,y} \[Element] Rectangle[{a,c},{b,d}]]
    !> J[0,b,c,d] // InputForm
    !> @endnote
    elemental function integrate_inv_r_on_rectangle_3(sup_x, inf_y, sup_y) result(integral)

        real(real64), intent(in) :: sup_x !! upper x boundary
        real(real64), intent(in) :: inf_y !! lower y boundary
        real(real64), intent(in) :: sup_y !! upper y boundary



        real(real64) :: integral



        associate(b => sup_x, c => inf_y, d => sup_y)

            associate( r_bc => hypot(b,c) , r_bd => hypot(b,d) )

                associate( r_bc_b => (r_bc + b), r_bd_b => (r_bd + b) )

                    integral = c * log( (c * c) / (r_bc_b * r_bc_b) ) &!
                    &        - d * log( (d * d) / (r_bd_b * r_bd_b) ) &!
                    &        + b * log( ( (r_bc - c) * (r_bd + d) ) / ( (r_bc + c) * (r_bd - d) ) )

                    integral = integral * 0.5_real64

                end associate

            end associate

        end associate

    end function integrate_inv_r_on_rectangle_3



    !> @note
    !> J[a_,b_,c_,d_]:=Integrate[1/Sqrt[x^2+y^2], {x,y} \[Element] Rectangle[{a,c},{b,d}]]
    !> J[a,b,c,d] // InputForm
    !> @endnote
    elemental function integrate_inv_r_on_rectangle_4(inf_x, sup_x, inf_y, sup_y) result(integral)

        real(real64), intent(in) :: inf_x !! lower x boundary
        real(real64), intent(in) :: sup_x !! upper x boundary
        real(real64), intent(in) :: inf_y !! lower y boundary
        real(real64), intent(in) :: sup_y !! upper y boundary



        real(real64) :: integral



        associate(a => inf_x, b => sup_x, c => inf_y, d => sup_y)

            associate( r_ac => hypot(a,c) , r_ad => hypot(a,d) , r_bc => hypot(b,c) , r_bd => hypot(b,d) )

                integral =              c * log( (r_ac + a) / (r_bc + b) ) &!
                &        +              d * log( (r_bd + b) / (r_ad + a) ) &!
                &        - 0.5_real64 * a * log( ( (r_ac - c)*(r_ad + d) ) / ( (c + r_ac) * (r_ad - d) ) ) &!
                &        + 0.5_real64 * b * log( ( (r_bc - c)*(r_bd + d) ) / ( (c + r_bc) * (r_bd - d) ) )

            end associate

        end associate

    end function integrate_inv_r_on_rectangle_4

  end module integrate_inv_r_on_rectangle_fortran
