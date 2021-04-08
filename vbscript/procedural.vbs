' To run from Windows command line: cscript.exe procedural.vbs

Dim sections(2)

' First section
Set sections(0) = CreateObject("Scripting.Dictionary")
sections(0).Add "title", "Getting started"
sections(0).Add "reset_lesson_position", False

Dim lessons1(1)

Set lessons1(0) = CreateObject("Scripting.Dictionary")
lessons1(0).Add "name", "Welcome"

Set lessons1(1) = CreateObject("Scripting.Dictionary")
lessons1(1).Add "name", "Installation"

sections(0).Add "lessons", lesson1

' Second section
Set sections(1) = CreateObject("Scripting.Dictionary")
sections(1).Add "title", "Basic operator"
sections(1).Add "reset_lesson_position", False

Dim lessons2(1)

Set lessons2(0) = CreateObject("Scripting.Dictionary")
lessons2(0).Add "name", "Addition / Subtraction"

Set lessons2(1) = CreateObject("Scripting.Dictionary")
lessons2(1).Add "name", "Multiplication / Division"

sections(1).Add "lessons", lesson2

' Third section
Set sections(2) = CreateObject("Scripting.Dictionary")
sections(2).Add "title", "Advanced topics"
sections(2).Add "reset_lesson_position", True

Dim lessons3(1)

Set lessons3(0) = CreateObject("Scripting.Dictionary")
lessons3(0).Add "name", "Mutability"

Set lessons3(1) = CreateObject("Scripting.Dictionary")
lessons3(1).Add "name", "Immutability"

sections(2).Add "lessons", lesson3

' Process
Dim section_position : section_position = 1
Dim lesson_position : lesson_position = 1

For Each section In sections
    section.Add "position", section_position
    section_position = section_position + 1

    If section("reset_lesson_position") = True Then
        lesson_position = 1
    End If

    For Each lesson In section("lessons")
        lesson("position") = lesson_position
        lesson_position = lesson_position + 1    
    Next
Next

' Print
For Each section In sections
    Wscript.Echo "Section Title: " & section("title")
    Wscript.Echo "Section Position: " & section("position")
    Wscript.Echo "Reset Lesson Position: " & section("reset_lesson_position")
    
    For Each lesson in section("lessons")
        Wscript.Echo "  " & "Lesson Name: " & lesson("name")
        Wscript.Echo "  " & "Lesson Position: " & lesson("position")
    Next

    Wscript.Echo ""
Next