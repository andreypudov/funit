!
! A unit testing library for Fortran
!
! Copyright 2011-2022 Andrey Pudov. All Rights Reserved.
!
! Licensed under the Apache License, Version 2.0.
! See LICENSE.txt in the project root for license information.
!

submodule (Logger) UnitLogger

    implicit none

contains
    module subroutine init_unitLogger(self)
        class(UnitLogger), intent(in out) :: self

        call cpu_time(self%start)
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
end submodule
