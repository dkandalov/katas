package ru.goos_gbt;

import com.objogate.wl.swing.AWTEventQueueProber;
import com.objogate.wl.swing.driver.JFrameDriver;
import com.objogate.wl.swing.driver.JLabelDriver;
import com.objogate.wl.swing.gesture.GesturePerformer;

import static org.hamcrest.Matchers.equalTo;

/**
 * User: dima
 * Date: 04/03/2012
 */
public class AuctionSniperDriver extends JFrameDriver {
    public AuctionSniperDriver(int timeoutMillis) {
        super(new GesturePerformer(),
                JFrameDriver.topLevelFrame(
                        named(Main.MAIN_WINDOW_NAME),
                        showingOnScreen()),
                new AWTEventQueueProber(timeoutMillis, 100)
        );
    }

    public void showsSniperStatus(String statusText) {
        new JLabelDriver(this, named(Main.SNIPER_STATUS_NAME)).hasText(equalTo(statusText));
    }
}
