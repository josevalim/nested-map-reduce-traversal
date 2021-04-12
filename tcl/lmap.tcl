#! /usr/bin/env tclsh
# Tested with Tcl 8.6 and Jim Tcl 0.80.

set sections {
    {
        title {Getting started}
        reset_lesson_position false
        lessons {
            {name Welcome}
            {name Installation}
        }
    }
    {
        title {Basic operator}
        reset_lesson_position false
        lessons {
            {name {Addition / Subtraction}}
            {name {Multiplication / Division}}
        }
    }
    {
        title {Advanced topics}
        reset_lesson_position true
        lessons {
            {name Mutability}
            {name Immutability}
        }
    }
}

set lessonCounter 1
set sectionCounter 1

puts [lmap section $sections {
    if {[dict get $section reset_lesson_position]} {
        set lessonCounter 1
    }

    dict set section position $sectionCounter
    incr sectionCounter

    dict set section lessons [lmap lesson [dict get $section lessons] {
        dict set lesson position $lessonCounter
        incr lessonCounter

        set lesson
    }]

    set section
}]
