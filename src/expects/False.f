!
! A unit testing library for Fortran
!
! Copyright 2011-2022 Andrey Pudov. All Rights Reserved.
!
! Licensed under the Apache License, Version 2.0.
! See LICENSE.txt in the project root for license information.
!

submodule (Unit) FalseExpects

    use Conditions

    implicit none

    character(len=*), parameter :: default = 'False condition'

contains
    module subroutine false_expect(condition, message)
        character(len=*), optional, intent(in) :: message
        logical, intent(in) :: condition

        if (.not. false(condition)) then
            call fail_expect(message, default)
        end if
    end subroutine
end submodule
