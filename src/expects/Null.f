!
! A unit testing library for Fortran
!
! Copyright 2011-2022 Andrey Pudov. All Rights Reserved.
!
! Licensed under the Apache License, Version 2.0.
! See LICENSE.txt in the project root for license information.
!

submodule (FUnit) NullExpects

    use Conditions

    implicit none

    character(len=*), parameter :: default = 'Null condition'

contains
    module subroutine null_expect_character(pointer, message)
        character(len=*), optional, intent(in) :: message
        character,        pointer,  intent(in) :: pointer

        if (.not. null(pointer)) then
            call fail_expect(message, default)
        end if
    end subroutine

    module subroutine null_expect_complex(pointer, message)
        character(len=*), optional, intent(in) :: message
        complex,          pointer, intent(in)  :: pointer

        if (.not. null(pointer)) then
            call fail_expect(message, default)
        end if
    end subroutine

    module subroutine null_expect_double_precision(pointer, message)
        character(len=*), optional, intent(in) :: message
        double precision, pointer,  intent(in) :: pointer

        if (.not. null(pointer)) then
            call fail_expect(message, default)
        end if
    end subroutine

    module subroutine null_expect_integer(pointer, message)
        character(len=*), optional, intent(in) :: message
        integer,          pointer,  intent(in) :: pointer

        if (.not. null(pointer)) then
            call fail_expect(message, default)
        end if
    end subroutine

    module subroutine null_expect_logical(pointer, message)
        character(len=*), optional, intent(in) :: message
        logical,          pointer,  intent(in) :: pointer

        if (.not. null(pointer)) then
            call fail_expect(message, default)
        end if
    end subroutine

    module subroutine null_expect_real(pointer, message)
        character(len=*), optional, intent(in) :: message
        real,             pointer,  intent(in) :: pointer

        if (.not. null(pointer)) then
            call fail_expect(message, default)
        end if
    end subroutine
end submodule
