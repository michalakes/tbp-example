#define NEBLK 10
#define LEBLK 32

module mod_domain
  implicit none
  type diffusion_t
    double precision,dimension(LEBLK,64,6) :: rhs
    type(elemblk_t), pointer :: elemblk
    contains
      procedure, NOPASS :: part2 => compute_diffusion_part2
  end type diffusion_t
  type elemblk_t
    integer :: esblk
    type(diffusion_t)  :: diffusion
  end type elemblk_t
  type grid_t
    integer :: grid_id   ! identifier for this grid, global domain is grid 1
    type(elemblk_t)    :: elemblk(NEBLK)
  end type grid_t
  type domain_t
    type( grid_t ) :: grid(1)
  end type domain_t
  interface
    module subroutine compute_diffusion_part2( this, es_task, ee_task )
      class (diffusion_t), target, intent(inout)    :: this
      integer,                     intent(in)       :: es_task, ee_task
    end subroutine compute_diffusion_part2
#ifdef WORKAROUND
    module subroutine y ( this, es_task, ee_task )
      class (diffusion_t), target, intent(inout)    :: this
      integer,                     intent(in)       :: es_task, ee_task
    end subroutine y
#endif
  end interface
#ifdef WORKAROUND
contains
  module procedure y !( this, es_task, ee_task )
    implicit none
    !class (diffusion_t), intent(in)    :: this
    !integer                            :: es_task, ee_task
    logical, external :: omp_is_initial_device
!$omp declare target
write(0,*)__LINE__,omp_is_initial_device(),this%elemblk%esblk,es_task
  end procedure y
#endif
end module mod_domain

module mod_neptune_model
  use mod_domain
  implicit none
  type neptune_t
    type ( domain_t ) :: domain
  end type neptune_t
end module mod_neptune_model

module mod_diffusion_driver
  implicit none
  contains
    subroutine diffusion_driver( d )
      use mod_domain, only : domain_t
      implicit none
      type(domain_t), target, intent(inout) :: d
      integer                    :: ie,ib
      logical, external :: omp_is_initial_device 
print*,'entering target region'
!$omp target
!$omp teams distribute parallel do private(ib,ie)
      do ib = 1, NEBLK
        do ie = 1, LEBLK
          if (ib.eq.1.and.ie.eq.1)write(0,*)omp_is_initial_device()
          call d%grid(1)%elemblk(ib)%diffusion%part2(d%grid(1)%elemblk(ib)%diffusion,      &
                        d%grid(1)%elemblk(ib)%esblk-1+ie,                                  &
                        d%grid(1)%elemblk(ib)%esblk-1+ie                                   )
        enddo
      enddo
!$omp end target
print*,'back from target region'
    end subroutine diffusion_driver
end module mod_diffusion_driver

program tbp
  use mod_neptune_model
  use mod_diffusion_driver
  implicit none
  type (neptune_t), target   :: model
  integer                    :: ie,ib
  logical, external :: omp_is_initial_device 
  do ib = 1, NEBLK
    model%domain%grid(1)%elemblk(ib)%esblk = (ib-1)*LEBLK+1
    model%domain%grid(1)%elemblk(ib)%diffusion%elemblk => model%domain%grid(1)%elemblk(ib)
    model%domain%grid(1)%elemblk(ib)%diffusion%rhs(1:LEBLK,64,6) = 0.
  enddo

  print*,'calling diffusion_driver'
  call diffusion_driver(model%domain)
  print*,'back from diffusion_driver'

  print*,model%domain%grid(1)%elemblk(1)%diffusion%rhs(1,1,1)
  print*
  print*,'  DONE  '
  print*
  stop

end program tbp
