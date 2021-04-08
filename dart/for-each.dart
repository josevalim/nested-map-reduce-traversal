
var sections = [
	{
		'title': 'Getting Started',
		'resetLessonPosition': false,
		'lessons': [
			{'name': 'Welcome'}, 
			{'name': 'Installation'}
		]
	},
	{
		'title': 'Basic Operator',
		'resetLessonPosition': false,
		'lessons': [
			{'name': 'Addition / Substraction'}, 
			{'name': 'Multiplication / Division'}
		]
	}, 
	{
		'title': 'Advanced Topics',
		'resetLessonPosition': true,
		'lessons': [
			{'name': 'Mutability'}, 
			{'name': 'Immutability'}
		]
	}
];

void main() {
	var sectionCounter = 0;
	var lessonCounter  = 0;

	for (var section in sections) {
		if (section['resetLessonPosition'] as bool) {
			lessonCounter = 0;
		}
		section['position'] = ++sectionCounter;

		for (var lesson in section['lessons'] as List) {
			lesson['position'] = (++lessonCounter).toString();
		}
	}
	print(sections);
}
