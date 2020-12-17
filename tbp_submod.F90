submodule (mod_domain) diffusion_submod
contains
  module procedure compute_diffusion_part2 ! ( this, es_task, ee_task )
    implicit none
!$omp declare target
    !class (diffusion_t), intent(in)    :: this
    !integer              intent(in)    :: es_task, ee_task
    ! local
    integer :: m, es,ee
    logical, external :: omp_is_initial_device
    ! Executable
    es = (es_task-this%elemblk%esblk+1)
    ee = (ee_task-this%elemblk%esblk+1)
write(0,*)omp_is_initial_device(),this%elemblk%esblk,es_task,es,ee
#ifdef WORKAROUND
    call y(this,es_task,ee_task)
#else
    call x(this,es_task,ee_task)
#endif
    do m=1,6
      this%rhs(es:ee,:,m) =  this%rhs(es:ee,:,m) + 0.2
    enddo
  end procedure compute_diffusion_part2

#ifdef WORKAROUND
#else
  subroutine x( this, es_task, ee_task )
    implicit none
    class (diffusion_t), intent(in)    :: this
    integer                            :: es_task, ee_task
    logical, external :: omp_is_initial_device
!$omp declare target
write(0,*)__LINE__,omp_is_initial_device(),this%elemblk%esblk,es_task
  end subroutine x
#endif

end submodule diffusion_submod

