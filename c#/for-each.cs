using System;
using System.Collections.Generic;



class Lesson {
 public String name { get; set; }
 public int position { get; set; }
	
	public Lesson(String _name, int _position) {
		name = _name;
		position = _position;
	}
}

class Section {
 public String title { get; set; }
 public Boolean reset_lesson_position { get; set; }
 public int position { get; set; }
 public List<Lesson> lessons { get; set; }
 
	public Section(String _title, Boolean _reset_lesson_position, int _position, List<Lesson> _lessons) {
		title = _title;
		reset_lesson_position = _reset_lesson_position;
		position  = _position;
		lessons = _lessons ;
	}
	
	public void Print() {
		Console.WriteLine("title, {0} reset_lesson {1},position {2} .", title, reset_lesson_position, position);
		
		foreach (Lesson l in lessons) {
				Console.WriteLine("name, {0} position {1}.", l.name, l.position);
			}
	}
}



public class Program
{
	public static void Main()
	{
		List<Section> sections = new List<Section>();
		List<Lesson> lessons = new List<Lesson>();
		sections.Add(new Section("Getting started", false, 0, new List<Lesson>() { new Lesson("Welcome", 0), new Lesson("Installation", 0) }));
		sections.Add(new Section("Basic operator", false, 0, new List<Lesson>() { new Lesson("Addition / Subtraction", 0), new Lesson("Multiplication / Division", 0) }));
		sections.Add(new Section("Advanced topics", true, 0, new List<Lesson>() { new Lesson("Mutability", 0), new Lesson("Immutability", 0) }));
		
		
		int lcount = 1;
		int scount = 1;
		foreach (Section s in sections) {
			if (s.reset_lesson_position) {
				lcount = 1;
			}
			s.position = scount;
			scount += 1;
			
			foreach (Lesson l in s.lessons) {
				l.position = lcount;
				lcount += 1;
			}
		}
		// printing
		foreach (Section s in sections) {
			s.Print();
		}
	}
}
