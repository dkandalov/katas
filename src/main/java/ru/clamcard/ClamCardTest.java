package ru.clamcard;

import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

public class ClamCardTest {
    private final static Station asterisk = new Station("Asterisk", Zone.A);
    private final static Station bison = new Station("Bison", Zone.B);
    private final static Station barbican = new Station("Barbican", Zone.B);
    private final static Station balham = new Station("Balham", Zone.B);
    private final static Station aldgate = new Station("Aldgate", Zone.A);
    private final static Station angel = new Station("Angel", Zone.A);

    private final ClamCard card = new ClamCard("Amir");

    @Test public void travel_from_zone_a_to_zone_a() {
        assertCharge(asterisk, aldgate, 250);
    }

    @Test public void travel_from_zone_a_to_zone_b() {
        assertCharge(asterisk, bison, 300);
    }

    @Test public void daily_cap_on_travelling_zone_a_and_b() {
        assertCharge(asterisk, barbican, 300);
        assertCharge(barbican, balham, 300);
        assertCharge(balham, bison, 200);
        assertCharge(bison, asterisk, 0);
    }

    @Test public void daily_cap_on_travelling_zone_a() {
        assertCharge(asterisk, angel, 250);
        assertCharge(angel, aldgate, 250);
        assertCharge(aldgate, asterisk, 200);
    }

    @Test public void daily_cap_on_is_reset_on_next_day() {
        assertCharge(asterisk, angel, 250, onDay(1));
        assertCharge(angel, aldgate, 250, onDay(1));
        assertCharge(aldgate, asterisk, 200, onDay(1));

        assertCharge(asterisk, angel, 250, onDay(2));
    }

    private void assertCharge(Station inStation, Station outStation, int charge) {
        assertCharge(inStation, outStation, charge, onDay(1));
    }

    private void assertCharge(Station inStation, Station outStation, int expectedCharge, Day day) {
        Money money = card.Calculate(inStation, outStation, day);
        assertThat(money, equalTo(new Money(expectedCharge)));
    }

    private static Day onDay(int i) {
        return new Day(i);
    }

    private static class Day {
        private final int n;

        public Day(int n) {
            this.n = n;
        }

        @Override public String toString() {
            return "Day{n=" + n + '}';
        }

        @Override public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;

            Day day = (Day) o;

            return n == day.n;
        }

        @Override public int hashCode() {
            return n;
        }
    }

    private static class ClamCard {
        private final String owner;
        private final List<Station> travelHistory;
        private Money amountSpent;
        private Day travelDay;

        public ClamCard(String owner) {

            this.owner = owner;
            this.amountSpent = new Money(0);
            this.travelDay = new Day(0);
            this.travelHistory = new ArrayList<>();
        }

        public Money Calculate(Station inStation, Station outStation, Day day) {
            travelHistory.add(inStation);
            travelHistory.add(outStation);

            if (!day.equals(this.travelDay)) {
                this.amountSpent = new Money(0);
                this.travelDay = day;
            }

            Money amount = outStation.zone == Zone.B ? new Money(300) : new Money(250);

            boolean wasInZoneB = travelHistory.stream().anyMatch(it -> it.zone == Zone.B);
            Money chargeCap = wasInZoneB ? new Money(800) : new Money(700);
            Money remainder = chargeCap.minus(amountSpent);
            Money money = remainder.greaterThan(amount) ? amount : remainder;

            amountSpent = amountSpent.plus(money);

            return money;
        }

        @Override public String toString() {
            return "ClamCard{owner='" + owner + '\'' + '}';
        }
    }

    private enum Zone {
        A, B
    }

    private static class Station {
        private final String name;
        public final Zone zone;

        public Station(String name, Zone zone) {
            this.name = name;
            this.zone = zone;
        }

        @Override public String toString() {
            return "Station{name='" + name + '\'' + ", zone=" + zone + '}';
        }
    }

    private static class Money {
        private final int amount;

        public Money(int amount) {
            this.amount = amount;
        }

        @Override public String toString() {
            return "Money{amount=" + amount + '}';
        }

        @Override public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;

            Money money = (Money) o;

            return amount == money.amount;

        }

        @Override public int hashCode() {
            return amount;
        }

        public Money plus(Money that) {

            return new Money(this.amount + that.amount);
        }

        public Money minus(Money that) {
            return new Money(this.amount - that.amount);
        }

        public boolean greaterThan(Money that) {
            return this.amount > that.amount;
        }
    }
}
