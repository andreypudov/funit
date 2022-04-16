!
! A unit testing library for Fortran
!
! Copyright 2011-2022 Andrey Pudov. All Rights Reserved.
!
! Licensed under the Apache License, Version 2.0.
! See LICENSE.txt in the project root for license information.
!

module Arguments

    implicit none
    private

    integer, parameter, public :: LOGGER_CONSOLE = 1
    integer, parameter, public :: LOGGER_JSON    = 2

    type, public :: ArgumentsParser
    private
        character(len=256) :: file
        integer :: type
    contains
        procedure, pass, public :: parse

        procedure, pass, public :: getLoggerType
        procedure, pass, public :: getLoggerFile
    end type
contains
    subroutine parse(self)
        class(ArgumentsParser), intent(in out) :: self

        character(len=80), dimension(:), allocatable :: arguments
        logical skip
        integer count
        integer index

        count = command_argument_count()
        skip  = .false.
        allocate(arguments(count))

        do index = 1, count
            if (skip) then
                skip = .false.
                cycle
            end if

            call get_command_argument(index, arguments(index))

            select case(arguments(index))
            case ('-v')
                call version()
            case ('--version')
                call version()
            case ('--json')
                if (count < index + 1) then
                    call help()
                end if

                call get_command_argument(index, arguments(index + 1))
                self%type = LOGGER_JSON
                self%file = arguments(index + 1)

                skip = .true.
            case default
                call help()
            end select
        end do

        deallocate(arguments)
    end subroutine

    function getLoggerType(self) result(value)
        class(ArgumentsParser), intent(in) :: self
        integer value

        value = self%type
    end function

    function getLoggerFile(self) result(value)
        class(ArgumentsParser), intent(in) :: self
        character(len=256) value

        value = self%file
    end function

    subroutine help()
        print '(A)', 'FUnit. A unit testing library for Fortran.'
        print '(A)', 'Copyright (C) 2011-2022 Andrey Pudov'
        print '(A)', ''
        print '(A)', 'Usage: unit [options]'
        print '(A)', ''
        print '(A)', 'Options:'
        print '(TR2,A,TR8,A)', '-h, --help',    'display this help message and exit'
        print '(TR2,A,TR5,A)', '-v, --version', 'display version information and exit'
        print '(TR2,A,TR7,A)', '--json file',   'output unit results to the JSON file'
        print '(A)', ''

        stop
    end subroutine

    subroutine version()
        print '(A)', 'FUnit. A unit testing library for Fortran.'
        print '(A)', 'Copyright (C) 2011-2022 Andrey Pudov'
        print '(A)', ''
        print '(A)', '0.00.00'
        print '(A)', ''

        stop
    end subroutine
end module
