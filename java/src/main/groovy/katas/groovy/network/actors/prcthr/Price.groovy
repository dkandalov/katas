package katas.groovy.network.actors.prcthr

/**
 * User: dima
 * Date: 15/11/2011
 */
@groovy.transform.Immutable
final class Price {
  double high
  double low

  public String toString() {
    return "Price{" +
            "high=" + high +
            ", low=" + low +
            '}';
  }
}
