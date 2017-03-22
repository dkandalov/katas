package katas.java.doors;

import org.junit.Test;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static java.util.Arrays.asList;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

/**
 * User: dima
 * Date: 14/4/11
 */
public class Ladder3 {
	@Test
	public void shouldWalkDoors() {
		assertThat(walk(1), equalTo(asList(false)));
		assertThat(walk(2), equalTo(asList(false, true)));
		assertThat(walk(3), equalTo(asList(false, true, true)));
		assertThat(walk(4), equalTo(asList(false, true, true, false)));
		assertThat(walk(5), equalTo(asList(false, true, true, false, true)));
		assertThat(walk(6), equalTo(asList(false, true, true, false, true, true)));
		assertThat(walk(7), equalTo(asList(false, true, true, false, true, true, true)));
		assertThat(walk(8), equalTo(asList(false, true, true, false, true, true, true, true)));
		assertThat(walk(9), equalTo(asList(false, true, true, false, true, true, true, true, false)));
		assertThat(walk(10), equalTo(asList(false, true, true, false, true, true, true, true, false, true)));
	}

	private List<Boolean> walk(int doorsNumber) {
		List<Boolean> doors = new ArrayList<Boolean>(doorsNumber); // didn't fill list correctly, was size == 0
		doors.addAll(Collections.nCopies(doorsNumber, true));

		for (int stepSize = 1; stepSize <= doorsNumber; stepSize++) {
			for (int i = stepSize - 1; i < doorsNumber; i += stepSize) {
				doors.set(i, !doors.get(i));
			}
		}
		return doors;
	}
}
