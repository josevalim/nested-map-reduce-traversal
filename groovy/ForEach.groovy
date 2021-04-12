#!/usr/bin/env groovy

/* 
   You can run it several ways:
   1) ./groovy ForEach.groovy # output to console
   2) ./groovyConsole ForEach.groovy # loaded in GUI for running and inspecting groovy scripts
   3) chmod +x ForEach.groovy; ./ForEach.groovy # run it as script and output to console
*/   

sections = 
[
  [
    "title": "Getting started",
    "reset_lesson_position": false,
    "lessons": [
      ["name": "Welcome"],
      ["name": "Installation"],
    ]
  ],

  [
    "title": "Basic operator",
    "reset_lesson_position": false,
    "lessons": [
      ["name": "Addition / Subtraction"],
      ["name": "Multiplication / Division"]
    ]
  ],

  [
    "title": "Advanced topics",
    "reset_lesson_position": true,
    "lessons": [
      ["name": "Mutability"],
      ["name": "Immutability"]
    ]
  ]
]

sectionCounter = 1
lessonCounter = 1

for (section in sections) {
    if (section.reset_lesson_position) {
        lessonCounter = 1        
    }    
    
    section.position = sectionCounter++   
       
    for (lesson in section.lessons) {
        lesson.position = lessonCounter++
    }
}


println(sections)

/* to easily compare to https://github.com/josevalim/nested-data-structure-traversal#the-problem expected output   */
//import groovy.json.JsonOutput;
//println JsonOutput.prettyPrint(JsonOutput.toJson(sections)) 


