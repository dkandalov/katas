package ru.goos_gbt;

/**
 * User: dima
 * Date: 17/03/2012
 */
public interface SniperListener {
    void sniperLost();

    void sniperStateChanged(SniperSnapshot sniperSnapshot);

    void sniperWinning();

    void sniperWon();
}
