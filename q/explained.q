
/ To use this you'll need kdb+:
/   https://code.kx.com/q/learn/install/
/ Run q:
/   $ l64/q
/ At the q) prompt type:
/   \l explained.q
/ You'll get an output file named 'output.json'

\c 80 800

/ Optionally, Read in the JSON and translate to a table
/ d:.j.k read1 `:input.json;
d:.j.k "[{\"title\":\"Getting started\",\"reset_lesson_position\":false,\"lessons\":[{\"name\":\"Welcome\",\"position\":1},{\"name\":\"Installation\",\"position\":2},{\"name\":\"Installation2\",\"position\":3}],\"position\":1},{\"title\":\"Basic operator\",\"reset_lesson_position\":false,\"lessons\":[{\"name\":\"Addition / Subtraction\",\"position\":4},{\"name\":\"Multiplication / Division\",\"position\":5},{\"name\":\"Multiplication / Division2\",\"position\":6},{\"name\":\"Multiplication / Division3\",\"position\":7}],\"position\":2},{\"title\":\"Advanced topics\",\"reset_lesson_position\":true,\"lessons\":[{\"name\":\"Mutability\",\"position\":1},{\"name\":\"Immutability\",\"position\":2}],\"position\":3},{\"title\":\"Getting started\",\"reset_lesson_position\":false,\"lessons\":[{\"name\":\"Welcome\",\"position\":3},{\"name\":\"Installation\",\"position\":4},{\"name\":\"Installation2\",\"position\":5}],\"position\":4},{\"title\":\"Basic operator\",\"reset_lesson_position\":true,\"lessons\":[{\"name\":\"Addition / Subtraction\",\"position\":1},{\"name\":\"Multiplication / Division\",\"position\":2},{\"name\":\"Multiplication / Division2\",\"position\":3},{\"name\":\"Multiplication / Division3\",\"position\":4}],\"position\":5},{\"title\":\"Advanced topics\",\"reset_lesson_position\":true,\"lessons\":[{\"name\":\"Mutability\",\"position\":1},{\"name\":\"Immutability\",\"position\":2}],\"position\":6}]";
/ q)d
/ title             reset_lesson_position lessons
/ -------------------------------------------------------------------------------------------------------------------------------------------------------------------
/ "Getting started" 0                     +(,`name)!,("Welcome";"Installation";"Installation2")
/ "Basic operator"  0                     +(,`name)!,("Addition / Subtraction";"Multiplication / Division";"Multiplication / Division2";"Multiplication / Division3")
/ "Advanced topics" 1                     +(,`name)!,("Mutability";"Immutability")
/ "Getting started" 0                     +(,`name)!,("Welcome";"Installation";"Installation2")
/ "Basic operator"  1                     +(,`name)!,("Addition / Subtraction";"Multiplication / Division";"Multiplication / Division2";"Multiplication / Division3")
/ "Advanced topics" 1                     +(,`name)!,("Mutability";"Immutability")
/ q)

/ Get the row indexes where we need to reset lesson position
rlpids:where (d`reset_lesson_position)=1b;

/ Cut the table at those index points into a list of sub-tables
rlpgroups:(0,rlpids)_d;

/ For each sub-table:
/   Get the sum of the count of all lessons in this group, and make a range (1..count)
/       cel:count each x
/      rc:1+til sum cel;
/   Reshape the range to match the number of lessons in each group
/     (0,1_cel-1)_rc
/     thus (1;2;3;4;5) to ((1;2;3);(4;5))
/   and add each sub-list to its related lesson (update lp:...)
dlp:{update lp:{cel:count each x; rc:1+til sum cel; (0,1_cel-1)_rc}[lessons] from x} each rlpgroups;

/ Merge the lp into the nested lessons tables
dlpi:{update lessons:{x,'([]position:y)}'[lessons;lp] from x} each dlp;

/ Merge the lp into the nested lessons tables
/ Flatten the list of tables
newd:raze dlpi;

/ Add the incrementing position for each class
newd:update position:1+til count newd from newd;

/ Remove our working field(s)
newd:delete lp from newd;

/ q)newd
/ title             reset_lesson_position lessons
/ ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
/ "Getting started" 0                     +`name`position!(("Welcome";"Installation";"Installation2");1 2 3)
/ "Basic operator"  0                     +`name`position!(("Addition / Subtraction";"Multiplication / Division";"Multiplication / Division2";"Multiplication / Division3");4 5 6 7)
/ "Advanced topics" 1                     +`name`position!(("Mutability";"Immutability");1 2)
/ "Getting started" 0                     +`name`position!(("Welcome";"Installation";"Installation2");3 4 5)
/ "Basic operator"  1                     +`name`position!(("Addition / Subtraction";"Multiplication / Division";"Multiplication / Division2";"Multiplication / Division3");1 2 3 4)
/ "Advanced topics" 1                     +`name`position!(("Mutability";"Immutability");1 2)
/ q)

/ Table to JSON
newjd:.j.j newd;

/ Output table and JSON to console
show newd;
show newjd;

/ Save to disk
`:output.json 0: enlist newjd;

