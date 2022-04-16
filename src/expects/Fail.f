!
! A unit testing library for Fortran
!
! Copyright 2011-2022 Andrey Pudov. All Rights Reserved.
!
! Licensed under the Apache License, Version 2.0.
! See LICENSE.txt in the project root for license information.
!

submodule (FUnit) FailExpects

    implicit none

contains
    module subroutine fail_expect(message, default)
        character(len=*), optional, intent(in) :: message
        character(len=*), optional, intent(in) :: default

        class(UnitCaseEntry), pointer :: case
        class(UnitLogger),    pointer :: logger
        type(UnitContext) context

        case   => context%getCase()
        logger => context%getLogger()

        case%status = .false.
        if (present(message)) then
            call logger%log(type = TYPE_CASE, name = case%name, details = message, status = case%status)
        else
            call logger%log(type = TYPE_CASE, name = case%name, details = default, status = case%status)
        end if
    end subroutine
end submodule
