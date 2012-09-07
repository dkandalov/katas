package ru.yahoofinance
import org.apache.commons.math.stat.descriptive.moment.StandardDeviation
import org.apache.commons.math.stat.descriptive.moment.Variance
import org.joda.time.format.DateTimeFormat
import ru.yahoofinance.quotes.Quote
import ru.yahoofinance.quotes.QuoteSource

import static java.lang.Double.NaN
import static java.lang.Double.isNaN

/**
 * User: dima
 * Date: 04/09/2012
 */
class IndicatorService {
  private static final String FROM = "01/01/2002"
  private static final String TO = "01/01/2010"
  private final QuoteSource quoteSource

  IndicatorService(QuoteSource quoteSource) {
    this.quoteSource = quoteSource
  }

  def openPricesFor(String symbol) {
    quotesFor(symbol).collect{ it.open }
  }

  def quotesFor(String symbol) {
    quoteSource.quotesFor(symbol, FROM, TO)
  }

  def varianceOf(String symbol, int period = 7) {
    calcIndicatorFor(symbol, new VarianceCalc(period))
  }

  def stdDeviationOf(String symbol, int period = 7) {
    calcIndicatorFor(symbol, new StdDeviation(period))
  }

  def emaOf(String symbol, int period = 7) {
    calcIndicatorFor(symbol, new EMACalc(period))
  }

  def macdOf(String symbol, int shortPeriod = 12, int longPeriod = 26) {
    calcIndicatorFor(symbol, new MACDCalc(shortPeriod, longPeriod))
  }

  def macdSignalOf(String symbol, int shortPeriod = 12, int longPeriod = 26) {
    calcIndicatorFor(symbol, new MACDSignal(shortPeriod, longPeriod))
  }

  def macdHistogramOf(String symbol, int shortPeriod = 12, int longPeriod = 26) {
    calcIndicatorFor(symbol, new MACDSignal(shortPeriod, longPeriod))
  }

  private def calcIndicatorFor(String symbol, indicator) {
    quoteSource.quotesFor(symbol, FROM, TO).collect { quote ->
      def value = indicator.calc(quote.close)
      new CalcResult(isNaN(value) ? 0 : value, quote)
    }
  }


  static class MACDHistogram {
    private final MACDSignal macdSignal
    private final MACDCalc macd

    MACDHistogram(int shortPeriod, int longPeriod) {
      macdSignal = new MACDSignal(shortPeriod, longPeriod)
      macd = new MACDCalc(shortPeriod, longPeriod)
    }

    def calc(double value) {
      macd.calc(value) - macdSignal.calc(value)
    }
  }

  static class MACDSignal {
    private final EMACalc ema = new EMACalc(9)
    private final MACDCalc macd

    MACDSignal(int shortPeriod, int longPeriod) {
      macd = new MACDCalc(shortPeriod, longPeriod)
    }

    def calc(double value) {
      ema.calc(macd.calc(value))
    }
  }

  static class MACDCalc {
    private final EMACalc emaCalcShort
    private final EMACalc emaCalcLong

    MACDCalc(int shortPeriod, int longPeriod) {
      emaCalcShort = new EMACalc(shortPeriod)
      emaCalcLong = new EMACalc(longPeriod)
    }

    def calc(double value) {
      def emaShort = emaCalcShort.calc(value)
      def emaLong = emaCalcLong.calc(value)
      emaShort - emaLong
    }
  }

  static class EMACalc {
    private final RingBuffer ringBuffer
    private final double weight

    EMACalc(int size, double weight = 2 / (size + 1)) {
      this.ringBuffer = new RingBuffer(size)
      this.weight = weight
    }

    def calc(double value) {
      if (isNaN(value)) return NaN

      ringBuffer.add(value)
      if (!ringBuffer.full) return NaN
      emaOf(ringBuffer.values(), weight)
    }

    private static double emaOf(double[] values, double weight) {
      double result = 0
      for (int i = 1; i < values.length; i++) {
        result += values[i] * weight * Math.pow(1 - weight, i)
      }
      result
    }
  }

  static class StdDeviation {
    private final RingBuffer ringBuffer
    private final StandardDeviation deviation = new StandardDeviation()

    StdDeviation(int size) {
      ringBuffer = new RingBuffer(size)
    }

    def calc(double value) {
      ringBuffer.add(value)
      if (ringBuffer.full) {
        deviation.evaluate(ringBuffer.values())
      } else {
        NaN
      }
    }
  }

  static class VarianceCalc {
    private final RingBuffer ringBuffer
    private final Variance variance = new Variance()

    VarianceCalc(int size) {
      ringBuffer = new RingBuffer(size)
    }

    def calc(double value) {
      ringBuffer.add(value)
      if (ringBuffer.full) {
        variance.evaluate(ringBuffer.values())
      } else {
        NaN
      }
    }
  }

  static class RingBuffer {
    private final int size
    private final List list = []

    RingBuffer(int size) {
      this.size = size
    }

    def add(double value) {
      list.add(value)
      if (list.size() > size) list.remove(0)
    }

    boolean isFull() { list.size() == size }

    double[] values() { list }
  }

  static class CalcResult {
    final double value
    final Quote quote

    CalcResult(double value, Quote quote) {
      this.value = value
      this.quote = quote
    }

    String toJSON() {
      "{ " +
              "\"value\": ${value}, " +
              "\"date\": \"${DateTimeFormat.forPattern("dd/MM/yyyy").print(quote.date)}\"" +
              "}"
    }

    @Override String toString() {
      "CalcResult{" +
              "value=" + value +
              ", quote.date=" + quote.date +
              '}'
    }

    @Override boolean equals(o) {
      if (is(o)) return true
      if (getClass() != o.class) return false

      CalcResult that = (CalcResult) o

      if (Double.compare(that.value, value) != 0) return false
      if (quote != that.quote) return false

      return true
    }

    @Override int hashCode() {
      int result
      long temp
      temp = value != +0.0d ? Double.doubleToLongBits(value) : 0L
      result = (int) (temp ^ (temp >>> 32))
      result = 31 * result + (quote != null ? quote.hashCode() : 0)
      return result
    }
  }
}
