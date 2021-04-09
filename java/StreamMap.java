import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

public class StreamMap {

	private static Collection<Section> sections;

	static {
		sections = List.of(
			new Section("Getting started", false, null, List.of(
					new Lesson("Welcome", null),
					new Lesson("Installation", null)
				)
			),
			new Section("Basic operator", false, null, List.of(
					new Lesson("Addition / Subtraction", null),
					new Lesson("Multiplication / Division", null)
				)
			),
			new Section("Advanced topics", true, null, List.of(
					new Lesson("Mutability", null),
					new Lesson("Immutability", null)
				)
			)
		);
	}

	public static void main(final String[] args) {
		final var indexedSections = new Indexer(sections).index();
		System.out.println(indexedSections);
	}

	record Lesson(String name, Integer position) {}

	record Section(String title, boolean resetLessonPosition, Integer position, Collection<Lesson> lessons) {}

	public static class Indexer {

		private final Collection<Section> sections;

		private int lessonIndex = 0;
		private int sectionIndex = 0;

		public Indexer(final Collection<Section> sections) {
			this.sections = sections;
		}

		public Collection<Section> index() {
			return sections.stream()
				.map(s -> {
					if (s.resetLessonPosition) lessonIndex = 0;
					return new Section(s.title, s.resetLessonPosition, ++sectionIndex, s.lessons.stream()
						.map(l -> {
							return new Lesson(l.name, ++lessonIndex);
						}).collect(Collectors.toUnmodifiableList()));
				}).collect(Collectors.toUnmodifiableList());
		}

	}

}
