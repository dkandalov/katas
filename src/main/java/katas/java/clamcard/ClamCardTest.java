package katas.java.clamcard;

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

    private final ClamCard card = new ClamCard("Amir Bazazi");


    @Test public void travel_from_zone_a_to_zone_a() {
        expectChargeOf(250, asterisk, aldgate);
    }

    @Test public void travel_from_zone_a_to_zone_b() {
        expectChargeOf(300, asterisk, bison);
    }

    @Test public void daily_cap_on_travelling_zone_a_and_b() {
        expectChargeOf(300, asterisk, barbican);
        expectChargeOf(300, barbican, balham);
        expectChargeOf(200, balham, bison);
        expectChargeOf(0, bison, asterisk);
    }

    @Test public void daily_cap_on_travelling_zone_a() {
        expectChargeOf(250, asterisk, angel);
        expectChargeOf(250, angel, aldgate);
        expectChargeOf(200, aldgate, asterisk);
    }

    @Test public void daily_cap_on_is_reset_on_next_day() {
        expectChargeOf(250, onDay(1), asterisk, angel);
        expectChargeOf(250, onDay(1), angel, aldgate);
        expectChargeOf(200, onDay(1), aldgate, asterisk);

        expectChargeOf(250, onDay(2), asterisk, angel);
    }

    private void expectChargeOf(int chargeInPence, Station inStation, Station outStation) {
        expectChargeOf(chargeInPence, onDay(1), inStation, outStation);
    }

    private void expectChargeOf(int chargeInPence, Day travelDay, Station inStation, Station outStation) {
        Money money = card.charge(inStation, outStation, travelDay);
        assertThat(money, equalTo(new Money(chargeInPence)));
    }

    private static Day onDay(int i) {
        return new Day(i);
    }


    private static class ClamCard {
        private final String owner;
        private final List<Station> travelHistory;
        private Money amountSpent;
        private Day lastTravelDay;

        public ClamCard(String owner) {
            this.owner = owner;
            this.travelHistory = new ArrayList<>();
            this.amountSpent = new Money(0);
            this.lastTravelDay = new Day(0);
        }

        public Money charge(Station inStation, Station outStation, Day day) {
            travelHistory.add(inStation);
            travelHistory.add(outStation);

            if (!day.equals(this.lastTravelDay)) {
                this.amountSpent = new Money(0);
                this.lastTravelDay = day;
            }

            Money charge = outStation.zone == Zone.B ? new Money(300) : new Money(250);

            boolean wasInZoneB = travelHistory.stream().anyMatch(it -> it.zone == Zone.B);
            Money chargeCap = wasInZoneB ? new Money(800) : new Money(700);
            Money remainder = chargeCap.minus(amountSpent);
            Money actualCharge = remainder.greaterThan(charge) ? charge : remainder;

            amountSpent = amountSpent.plus(actualCharge);

            return actualCharge;
        }

        @Override public String toString() {
            return "ClamCard{" +
                    "owner='" + owner + '\'' +
                    ", travelHistory=" + travelHistory +
                    ", amountSpent=" + amountSpent +
                    ", lastTravelDay=" + lastTravelDay +
                    '}';
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
}
