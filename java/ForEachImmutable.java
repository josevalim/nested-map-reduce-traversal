import java.util.ArrayList;
import java.util.List;
import java.util.OptionalInt;

record Lesson(String name, OptionalInt position) {
    Lesson(String name) {
        this(name, OptionalInt.empty());
    }

    Lesson withPosition(int position) {
        return new Lesson(name(), OptionalInt.of(position));
    }
}

record Section(String title, boolean resetLessonPosition, List<Lesson> lessons,
               OptionalInt position) {
    Section(String title, boolean resetLessonPosition, List<Lesson> lessons) {
        this(title, resetLessonPosition, lessons, OptionalInt.empty());
    }

    Section withPosition(int position) {
        return new Section(title, resetLessonPosition, lessons, OptionalInt.of(position));
    }

    Section withLessons(List<Lesson> lessons) {
        return new Section(title, resetLessonPosition, lessons, position);
    }
}

class ForEachImmutable {

    private static final List<Section> sections = List.of(
            new Section("Getting started", false, List.of(
                    new Lesson("Welcome"),
                    new Lesson("Installation")
            )),
            new Section("Basic operator", false, List.of(
                    new Lesson("Addition / Subtraction"),
                    new Lesson("Multiplication / Division")
            )),
            new Section("Advanced topics", true, List.of(
                    new Lesson("Mutability"),
                    new Lesson("Immutability")
            ))
    );
    private static List<Section> traverse(List<Section> sections) {
        var sectionCounter = 1;
        var lessonCounter = 1;
        final var result = new ArrayList<Section>(sections.size());
        for (Section section : sections) {
            if (section.resetLessonPosition()) {
                lessonCounter = 1;
            }

            var updatedLessons = new ArrayList<Lesson>(section.lessons().size());
            for (Lesson lesson : section.lessons()) {
                updatedLessons.add(lesson.withPosition(lessonCounter));
                lessonCounter++;
            }

            result.add(section
                    .withPosition(sectionCounter)
                    .withLessons(updatedLessons));
            sectionCounter++;
        }
        return result;
    }

    public static void main(String[] args) {
        for (Section section : traverse(sections)) {
            System.out.println(section);
        }
    }

}
