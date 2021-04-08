import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

public class ForEach {
    public static void main(String[] args) {
        List<Section> sections = createSections();

        SectionsAccumulator accumulator = new SectionsAccumulator();
        
        sections.forEach(section -> {
            section.setPosition(accumulator.getSectionCounter(section.isResetLessonPosition()));
            section.getLessons().forEach(
                    lesson -> lesson.setPosition(accumulator.getLessonCounter())
            );
        });

        sections.forEach(System.out::println);
    }

    private static List<Section> createSections() {
        return new ArrayList<>(Arrays.asList(
                new Section()
                        .setTitle("Getting started")
                        .setLessons(
                                Arrays.asList(
                                        new Lesson().setName("Welcome"),
                                        new Lesson().setName("Installation")
                                )
                        ),
                new Section()
                        .setTitle("Basic operator")
                        .setLessons(
                                Arrays.asList(
                                        new Lesson().setName("Addition / Subtraction"),
                                        new Lesson().setName("Multiplication / Division")
                                )
                        ),
                new Section()
                        .setTitle("Advanced topics")
                        .setResetLessonPosition(true)
                        .setLessons(
                                Arrays.asList(
                                        new Lesson().setName("Mutability"),
                                        new Lesson().setName("Immutability")
                                )
                        )
        ));
    }

}

class SectionsAccumulator {
    private final AtomicInteger sectionCounter = new AtomicInteger();
    private final AtomicInteger lessonCounter = new AtomicInteger();

    public Integer getSectionCounter(boolean resetLessonCounter) {
        if (resetLessonCounter) {
            lessonCounter.set(0);
        }
        return sectionCounter.incrementAndGet();
    }

    public Integer getLessonCounter() {
        return lessonCounter.incrementAndGet();
    }
}

class Section {
    private String title;
    private boolean resetLessonPosition;
    private Integer position;
    private List<Lesson> lessons;

    public String getTitle() {
        return title;
    }

    public Section setTitle(String title) {
        this.title = title;
        return this;
    }

    public boolean isResetLessonPosition() {
        return resetLessonPosition;
    }

    public Section setResetLessonPosition(boolean resetLessonPosition) {
        this.resetLessonPosition = resetLessonPosition;
        return this;
    }

    public Integer getPosition() {
        return position;
    }

    public Section setPosition(Integer position) {
        this.position = position;
        return this;
    }

    public List<Lesson> getLessons() {
        if (lessons == null) {
            return Collections.emptyList();
        }
        return lessons;
    }

    public Section setLessons(List<Lesson> lessons) {
        this.lessons = lessons;
        return this;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("Section{");
        sb.append("title='").append(title).append('\'');
        sb.append(", resetLessonPosition=").append(resetLessonPosition);
        sb.append(", position=").append(position);
        sb.append(", lessons=").append(lessons);
        sb.append('}');
        return sb.toString();
    }
}

class Lesson {
    private String name;
    private Integer position;

    public String getName() {
        return name;
    }

    public Lesson setName(String name) {
        this.name = name;
        return this;
    }

    public Integer getPosition() {
        return position;
    }

    public Lesson setPosition(Integer position) {
        this.position = position;
        return this;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("Lesson{");
        sb.append("name='").append(name).append('\'');
        sb.append(", position=").append(position);
        sb.append('}');
        return sb.toString();
    }
}
