submodule (mod_domain) diffusion_submod
contains
  module procedure compute_diffusion_part2 ! ( this, es_task, ee_task )
    implicit none
    !$omp declare target
    !class (diffusion_t), intent(in)    :: this
    !integer              intent(in)    :: es_task, ee_task
    ! local
    integer :: m, es,ee
    ! Executable
    es = (es_task-this%elemblk%esblk+1)
    ee = (ee_task-this%elemblk%esblk+1)
    do m=1,6
      this%rhs(es:ee,:,m) =  this%rhs(es:ee,:,m) + 0.2
    enddo
  end procedure compute_diffusion_part2
end submodule diffusion_submod

