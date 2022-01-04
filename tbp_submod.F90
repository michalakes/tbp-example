submodule (mod_domain) diffusion_submod
contains
  module procedure compute_diffusion_part2 ! ( this, ib, es_task, ee_task )
    use omp_lib
    implicit none
!$omp declare target
    !class (diffusion_t), intent(in)    :: this
    !integer              intent(in)    :: ib
    !integer              intent(in)    :: es_task, ee_task
    ! local
    integer :: m, es,ee
    double precision, dimension(LEBLK)  :: rhs_loc
    !logical, external :: omp_is_initial_device
    ! Executable
    es =  (es_task-this%elemblk%esblk+1)
    ee =  (ee_task-this%elemblk%esblk+1)
!    call x(this,es_task,ee_task)
    do m=1,1
       rhs_loc(es:ee) = ee + 10000*es + 10000000*ib
      this%rhs(es:ee,1,m) =  rhs_loc(es:ee) ! ee + 10000*es + 10000000*ib ! this%rhs(es:ee,1,m) + 0.2
    enddo
  end procedure compute_diffusion_part2

  subroutine x( this, es_task, ee_task )
    use omp_lib
    implicit none
    class (diffusion_t), intent(in)    :: this
    integer                            :: es_task, ee_task
!$omp declare target
  end subroutine x

end submodule diffusion_submod

