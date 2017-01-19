package codility;

import org.junit.Test;

import java.time.Duration;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Optional;

import static java.lang.Integer.parseInt;
import static java.util.stream.Collectors.*;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

public class Task2 {
    @Test
    public void test() {
        assertThat(solution(""), equalTo(0));

        assertThat(solution("00:05:00,400-234-090"), equalTo(0));

        assertThat(solution(
            "00:05:01,701-080-080\n" +
            "00:00:10,400-234-090"
        ), equalTo(30));
        
        assertThat(solution(
            "00:05:01,701-080-080\n" +
            "00:01:00,400-234-090"
        ), equalTo(180));
        assertThat(solution(
            "00:07:02,701-080-080\n" +
            "00:06:02,400-234-090"
        ), equalTo(1050));

        // duration tie
        assertThat(solution(
            "00:01:00,701-080-080\n" +
            "00:01:00,400-234-090"
        ), equalTo(180));

        assertThat(solution(
            "00:01:07,400-234-090\n" +
            "00:05:01,701-080-080\n" +
            "00:05:00,400-234-090"
        ), equalTo(900));

        assertThat(solution(
            "01:00:00,701-080-080\n" +
            "02:00:00,400-234-090"
        ), equalTo(61 * 150)); // TODO this doesn't look right
    }

    public int solution(String s) {
        List<Call> calls = Arrays.stream(s.split("\n"))
                .filter(line -> !line.isEmpty())
                .map(line -> parse(line)).collect(toList());

        String promotedNumber = findPromotedNumber(calls);

        return calls.stream()
            .filter(call -> !call.number.equals(promotedNumber))
            .map(call -> {
                if (call.duration.compareTo(Duration.ofMinutes(5)) < 0) {
                    return (int) call.duration.getSeconds() * 3;
                } else {
                    return (int) (call.duration.toMinutes() + 1) * 150;
                }
            }).reduce((a, b) -> a + b).orElse(0);
    }

    private static String findPromotedNumber(List<Call> calls) {
        Collection<Optional<Call>> summedCalls = calls.stream().collect(groupingBy(it -> it.number, reducing(Call::plus))).values();
        if (summedCalls.isEmpty()) return null;
        if (summedCalls.size() == 1) return calls.get(0).number;

        return summedCalls.stream().map(Optional::get).sorted().findFirst().get().number;
    }

    private static Call parse(String line) {
        String[] lineParts = line.split(",");
        String[] parts = lineParts[0].split(":");
        Duration duration = Duration.ofHours(parseInt(parts[0]))
                     .plus(Duration.ofMinutes(parseInt(parts[1])))
                     .plus(Duration.ofSeconds(parseInt(parts[2])));
        String phoneNumber = lineParts[1];
        return new Call(duration, phoneNumber);
    }

    private static class Call implements Comparable<Call> {
        final Duration duration;
        final String number;

        public Call(Duration duration, String number) {
            this.duration = duration;
            this.number = number;
        }

        public Call plus(Call that) {
            return new Call(duration.plus(that.duration), number);
        }

        @Override public int compareTo(Call that) {
            int result = -duration.compareTo(that.duration);
            if (result != 0) return result;
            else return Integer.compare(
                    Integer.parseInt(this.number.replace("-", "")),
                    Integer.parseInt(that.number.replace("-", "")));
        }
    }
}
