immutable JSON = q{
	[
	  {
		"title": "Getting started",
		"reset_lesson_position": false,
		"lessons": [
		  {"name": "Welcome"},
		  {"name": "Installation"}
		]
	  },

	  {
		"title": "Basic operator",
		"reset_lesson_position": false,
		"lessons": [
		  {"name": "Addition / Subtraction"},
		  {"name": "Multiplication / Division"}
		]
	  },

	  {
		"title": "Advanced topics",
		"reset_lesson_position": true,
		"lessons": [
		  {"name": "Mutability"},
		  {"name": "Immutability"}
		]
	  }
	]
};

auto insertPositions(string jsonStr)
{
	import std.json : parseJSON, JSONValue;

	auto sectionCount = 1;
	auto lessonCount = 1;

	auto json = jsonStr.parseJSON;
	foreach(section; json.array)
	{
		if (section["reset_lesson_position"].boolean) 
			lessonCount = 1;

		section.object["position"] = JSONValue(sectionCount++);
					
		foreach(lesson; section["lessons"].array)
			lesson.object["position"] = JSONValue(lessonCount++);	
	}
	return json;
}


void main()
{
	import std.stdio : writeln;

	immutable jsonWithPositions = JSON.insertPositions;	// compile time function evaluation

	writeln(jsonWithPositions.toPrettyString);	
}
