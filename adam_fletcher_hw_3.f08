! Name: Adam_Fletcher
! Date: 02/11/2022
! Purpose: To calculate the relativistic and non-relativistic kinetic energies  &
!          to compare their values. As the velocity in each system approaches   &
!          the speed of light, the energies agree more. However, in the non-    &
!          relativistic limit, they do not agree. This is because the velocity  &
!          is nearly negligible compared to the speed of light so the Lorentz   &
!          transformation becomes 1.0, which then yields a relativistic energy  &
!          of 0.0 when input into the equation below.


program nr_r_kinetic_energies
  implicit none

                          ! Variable Dictionary
                                  
  real, parameter :: C = 299792458 ! Constant value for speed of light in 'm/s'
  real :: mass                     ! Variable of the mass in 'kg'
  real :: velocity                 ! Variable of the velocity in 'm/s'
  real :: gamma                    ! Variable of the Lorentz factor, unitless
  real :: ke_nr                    ! Non-relativistic kinetic energy in Joules
  real :: ke_r                     ! Relativistic kinetic energy in Joules
  

                                   ! Prompts user to input mass
  write (*,*) "Please enter the mass of the object (in kg):"                 
  read (*,*) mass
                                   ! Prompts user to input velocity
  write (*,*) "Please enter the velocity of the object (in m/s):"
  read (*,*) velocity

                                   ! Calculates gamma when velocity is inputed
  gamma = 1.0 / (1.0 - velocity**2 / C**2)**(1.0 / 2.0)

                                   ! Calculates non-relativistic kinetic energy
  ke_nr = (1.0 / 2.0) * mass * velocity**2
  
                                   ! Calculates relativistic kinetic energy
  ke_r = mass * C**2 * (gamma - 1.0)     


                                   ! Outputs calculated kinetic energies
  write (*,*) "The non-relativistic kinetic energy is:", ke_nr, "J."
  write (*,*) "The relativistic kinetic energy is:", ke_r, "J."

  

 stop 0                           ! Stops program execution

end program nr_r_kinetic_energies
