program demo

    use, intrinsic :: iso_fortran_env , only: compiler_options
    use, intrinsic :: iso_fortran_env , only: compiler_version
    use, intrinsic :: iso_fortran_env , only: real64

    use, non_intrinsic :: integrate_inv_r_on_rectangle_fortran, only: integrate => integrate_inv_r_on_rectangle



    implicit none



    real(real64), parameter :: node(21) = &!
        [ &!
        +0.0000000000000000000000000000000000000e+0_real64 , &!
        +7.7510137936254275593910046932238211118e-2_real64 , &!
        +1.5455412068384000053764177103260527645e-1_real64 , &!
        +2.3066859655953025884907638463378394249e-1_real64 , &!
        +3.0539580402944574212481097369096142034e-1_real64 , &!
        +3.7828632473108320465961787004635794792e-1_real64 , &!
        +4.4890178631482849895962825156145213423e-1_real64 , &!
        +5.1681749884712931697752475021579406471e-1_real64 , &!
        +5.8162500891521268802559775686552250788e-1_real64 , &!
        +6.4293455606458542287698885309606499416e-1_real64 , &!
        +7.0037741678068295779539323766897166166e-1_real64 , &!
        +7.5360812188695136491489962318809721982e-1_real64 , &!
        +8.0230653395809178141620762605923580306e-1_real64 , &!
        +8.4617977210399184221877112394233139321e-1_real64 , &!
        +8.8496397216961512278033556586677014522e-1_real64 , &!
        +9.1842587069790381319985818376927549390e-1_real64 , &!
        +9.4636419964749212120053402434768160323e-1_real64 , &!
        +9.6861086956768183006593653914084092944e-1_real64 , &!
        +9.8503185912472477625390605699225951112e-1_real64 , &!
        +9.9552712744528436148800935977215467650e-1_real64 , &!
        +1.0000000000000000000000000000000000000e+0_real64   &!
        ]


    print '(A)', '[version]', compiler_version()
    print '(A)', '[options]', compiler_options()

    call run_demo( 1.0_real64, 'demo1.dat' )
    call run_demo( 2.0_real64, 'demo2.dat' )
    call run_demo( 4.0_real64, 'demo4.dat' )



    contains



    subroutine run_demo(ratio, file)

        real(real64), intent(in) :: ratio

        character(*), intent(in) :: file



        integer :: ix, iy

        integer :: file_unit

        real(real64) :: integral( size(node) )



        open( &!
        newunit = file_unit            , &!
        file    = ('example/' // file) , &!
        action  = 'write'              , &!
        form    = 'formatted'            &!
        )



        write( file_unit, '(I24,*(ES24.16E2))' ) size( node(:) ), ( ratio * node(:) )

        do iy = 1, size( node(:) )

            do concurrent ( ix = 1 : size( node(:) ) )
                
                integral(ix) = &!
                    integrate( &!
                    inf_x = ( -1.0_real64 - node(ix) ) * ratio , &!
                    sup_x = (  1.0_real64 - node(ix) ) * ratio , &!
                    inf_y = ( -1.0_real64 - node(iy) )         , &!
                    sup_y = (  1.0_real64 - node(iy) )           &!
                    )

            end do

            write( file_unit, '(*(ES24.16E2))' ) node(iy), integral(:)

        end do



        close(file_unit)

    end subroutine run_demo

end program demo
