import java.util.ArrayList;
import java.util.List;

/**
 * 
 * @author Marcelo Leite
 */
public class For {

	public static void main(String[] args) {
	
		List<Section> sections = loadData();
		
		int lessonPosition = 1;

		for (int i = 1; i <= sections.size(); i++) {
			
			boolean lessonPositionAlreadyReset = false;
			
			Section section = sections.get(i-1);
			section.setPosition(i);
			
			List<Lesson> lessons = section.getLessons();
			
			for (int j = 1; j <= lessons.size(); j++) {
				
				Lesson lesson = lessons.get(j-1);
				
				if (section.isResetLessonPosition() && !lessonPositionAlreadyReset) {
					lessonPosition = 1;
					lessonPositionAlreadyReset = true;
				}
				
				lesson.setPosition(lessonPosition);
				
				lessonPosition++;
			}
		}
		
		printOutput(sections);
	}

	private static void printOutput(List<Section> sections) {
	
		StringBuilder output = new StringBuilder();
		
		output.append("[").append("\n");
		
		sections.stream().forEach(section -> {

			output.append("\t{").append("\n");

			output.append("\t\t\"title\": ").append("\"").append(section.getTitle()).append("\",\n");
			output.append("\t\t\"reset_lesson_position\": ").append(section.isResetLessonPosition()).append(",").append("\n");
			output.append("\t\t\"position\": ").append(section.getPosition()).append(",").append("\n");
			
			output.append("\t\t\"lessons\": ").append("[").append("\n");
			
			section.getLessons().stream().forEach(lesson -> {
				output.append("\t\t\t{\"name\": ").append("\"").append(lesson.getName())
				.append("\", \"position\": ").append(lesson.getPosition()).append("},\n");				
			});
			
			output.append("\t\t]").append("\n");
			
			output.append("\t}").append(",").append("\n");
			
		});
		
		output.append("]");
		
		
		System.out.println(output.toString());
	}
	
	private static List<Section> loadData() {
		
		List<Section> sections = new ArrayList<>();
		
		
		Section section1 = new Section();
		section1.setTitle("Getting started");
		section1.getLessons().add(new Lesson("Welcome"));
		section1.getLessons().add(new Lesson("Installation"));
		
		Section section2 = new Section();
		section2.setTitle("Basic operator");
		section2.getLessons().add(new Lesson("Addition / Subtraction"));
		section2.getLessons().add(new Lesson("Multiplication / Division"));
		
		Section section3 = new Section();
		section3.setTitle("Advanced topics");
		section3.setResetLessonPosition(true);
		section3.getLessons().add(new Lesson("Mutability"));
		section3.getLessons().add(new Lesson("Immutability"));
		
		sections.add(section1);
		sections.add(section2);
		sections.add(section3);
		
		return sections;
	}
	
	private static class Section {
		private String title;
		private int position;
		private boolean resetLessonPosition;
		private List<Lesson> lessons = new ArrayList<>();

		public String getTitle() {
			return title;
		}
		public void setTitle(String title) {
			this.title = title;
		}
		public int getPosition() {
			return position;
		}
		public void setPosition(int position) {
			this.position = position;
		}
		public boolean isResetLessonPosition() {
			return resetLessonPosition;
		}
		public void setResetLessonPosition(boolean resetLessonPosition) {
			this.resetLessonPosition = resetLessonPosition;
		}
		public List<Lesson> getLessons() {
			return lessons;
		}
		public void setLessons(List<Lesson> lessons) {
			this.lessons = lessons;
		}
	}
	
	private static class Lesson {
		private String name;
		private int position;
		
		public Lesson(String name){
			this.name = name;
		}
		public String getName() {
			return name;
		}
		public void setName(String name) {
			this.name = name;
		}
		public int getPosition() {
			return position;
		}
		public void setPosition(int position) {
			this.position = position;
		}
	}
}
