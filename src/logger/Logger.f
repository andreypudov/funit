!
! A unit testing library for Fortran
!
! Copyright 2011-2022 Andrey Pudov. All Rights Reserved.
!
! Licensed under the Apache License, Version 2.0.
! See LICENSE.txt in the project root for license information.
!

module Logger

    implicit none
    private

    integer, parameter, public :: TYPE_RUNNER    = 0
    integer, parameter, public :: TYPE_SUITE     = 1
    integer, parameter, public :: TYPE_CASE      = 2
    integer, parameter, public :: TYPE_RESULT    = 3

    type, public :: UnitLogger
    private
        integer :: case
        integer :: passed
        integer :: failed

        real :: start  = 0.0 ! suite execution time
        real :: finish = 0.0 ! case execution time
    contains
        procedure, pass, public :: init  => init_unitLogger
        procedure, pass, public :: clean => clean_unitLogger

        procedure, pass, public :: log => log_unitLogger
    end type

    type, extends(UnitLogger), public :: ConsoleLogger
    private
    contains
        procedure, pass, public :: init  => init_consoleLogger
        procedure, pass, public :: clean => clean_consoleLogger

        procedure, pass, public :: log => log_consoleLogger
    end type

    type, extends(UnitLogger), public :: JSONLogger
    private
    contains
        procedure, pass, public :: init  => init_JSONLogger
        procedure, pass, public :: clean => clean_JSONLogger

        procedure, pass, public :: log => log_JSONLogger
    end type

    interface
        module subroutine init_unitLogger(self)
            class(UnitLogger), intent(in out) :: self
        end subroutine

        module subroutine clean_unitLogger(self)
            class(UnitLogger), intent(in out) :: self
        end subroutine

        module subroutine log_unitLogger(self, type, name, details, status)
            class(UnitLogger),          intent(in out) :: self
            integer,                    intent(in)     :: type
            character(len=*),           intent(in)     :: name
            character(len=*), optional, intent(in)     :: details
            logical,          optional, intent(in)     :: status
        end subroutine
    end interface

    interface
        module subroutine init_consoleLogger(self)
            class(ConsoleLogger), intent(in out) :: self
        end subroutine

        module subroutine clean_consoleLogger(self)
            class(ConsoleLogger), intent(in out) :: self
        end subroutine

       module subroutine log_consoleLogger(self, type, name, details, status)
           class(ConsoleLogger),       intent(in out) :: self
           integer,                    intent(in)     :: type
           character(len=*),           intent(in)     :: name
           character(len=*), optional, intent(in)     :: details
           logical,          optional, intent(in)     :: status
       end subroutine
   end interface

   interface
        module subroutine init_JSONLogger(self)
            class(JSONLogger), intent(in out) :: self
        end subroutine

        module subroutine clean_JSONLogger(self)
            class(JSONLogger), intent(in out) :: self
        end subroutine

       module subroutine log_JSONLogger(self, type, name, details, status)
           class(JSONLogger),          intent(in out) :: self
           integer,                    intent(in)     :: type
           character(len=*),           intent(in)     :: name
           character(len=*), optional, intent(in)     :: details
           logical,          optional, intent(in)     :: status
       end subroutine
    end interface
end module
